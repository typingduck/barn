package barn

object BarnHdfsWriter extends App {

  import Stream.continually
  import scalaz._
  import Scalaz._
  import Logging._
  import BarnSteps._

  val (conf, rootLogDir, baseHdfsDir) = loadConf(args)

  continually(() => listSubdirectories(rootLogDir)).iterator.foreach {
    _().toOption.fold( _.foreach { serviceDir =>
      info("Checking service " + serviceDir + " to sync.")

      val hdfsDir = baseHdfsDir + serviceDir.getName
      val hdfsTempDir = baseHdfsDir + serviceDir.getName + "/tmp/"

      val localTempDir = "/tmp"

      val excludeLocal = List(".gitignore", "target", "lock", "current")
      val excludeHdfs = List("tmp")

      val shipInterval = 10 //in seconds
      val retention = 60

      val result = for {
        fs           <- HDFS.createFileSystem(conf)
        localFiles   <- listLocalFiles(serviceDir, excludeLocal)
        _            <- ensureHdfsDir(fs, hdfsDir)
        _            <- ensureHdfsDir(fs, hdfsTempDir)
        hdfsFiles    <- listHdfsFiles(fs, hdfsDir, excludeHdfs)
        candidates   <- outstandingFiles(localFiles, hdfsFiles, shipInterval)
        concatted    <- concatCandidates(candidates, localTempDir)
        hdfsTempFile <- shipToHdfs(fs, concatted, hdfsTempDir)
        _            <- atomicRenameOnHdfs(fs, hdfsTempFile, hdfsDir)
        cleanupLimit <- earliestTimestamp(candidates)
        ♬            <- cleanupLocal( serviceDir
                                    , retention
                                    , cleanupLimit
                                    , excludeLocal )
      } yield ♬     // ♬ = tada! -- very limited in scope, hence the name.

      result ||| error

    }, error _)
  }
}

object BarnSteps {

  import java.io.File
  import scala.util.control.Exception._
  import org.joda.time._
  import org.apache.commons.lang.exception.ExceptionUtils._
  import org.apache.hadoop.fs.{Path => HdfsFile, FileSystem => HdfsFileSystem}
  import org.apache.hadoop.conf.Configuration

  import scalaz._
  import Scalaz._

  import Logging._
  import scala.math.Ordering

  /*
    File/Dir/Path on Local/Hdfs legend for the confusionary situation:

    type Dir -> local directory
    type File -> local File
    type HdfsDir -> hdfs directory
    type HdfsFile -> hdfs file

    the word "path is intentionally avoided as much as possible.
  */
  type Dir = File
  type HdfsDir = HdfsFile

  val lineDelim = System.getProperty("line.separator")

  def validate[U](body: => Validation[String,U], detail: String = null)
  : Validation[String, U]
  = allCatch either body fold ( exception =>
    detail match {
      case null => getStackTrace(exception).fail
      case sth  => (detail + lineDelim + getStackTrace(exception)).fail
    } , identity)

  def cleanupLocal(dir: Dir,
                   localRetention: Int,
                   cleanupLimit: DateTime,
                   exclude: List[String] = List.empty)
  : Validation[String, Unit]
  = for {
      localFiles <- listLocalFiles(dir, exclude)
      numDeleted <- validate(
          localFiles.foldLeft(0) { (deletedSoFar, file) =>
          val timestamp = Tai64.convertTai64ToTime(file.getName.drop(1))

          enoughTimePast(timestamp, localRetention) &&
            timestamp.isBefore(cleanupLimit) match {
            case true => file.delete; deletedSoFar + 1
            case false => deletedSoFar
          }

        }.success, "Deletion of retained files failed.")
    } yield tap(numDeleted) (x => info (x + " retained files deleted."))

  def listSubdirectories(dir: Dir)
  : Validation[String, List[Dir]]
  = validate(dir.listFiles.toList.filter(x => x.isDirectory).success,
    "Can't get list of local services directories on: " + dir)

  implicit object HdfsFileOrderingByName extends Ordering[HdfsFile] {
    def compare(o1: HdfsFile, o2: HdfsFile) = o1.getName compare o2.getName
  }

  def earliestTimestamp(localFiles: List[File])
  : Validation[String, DateTime]
  = Tai64.convertTai64ToTime(localFiles.head.getName.drop(1)).success


  def outstandingFiles(localFiles: List[File],
                       hdfsFiles: List[HdfsFile],
                       shippingIntervalInSeconds: Int)
  : Validation[String, List[File]]
  = hdfsFiles.sorted.lastOption match {
    case Some(last) =>
      val lastTimestamp = Tai64.convertTai64ToTime(last.getName.drop(1))
      localFiles.sorted.dropWhile(_.getName <= last.getName) match {
        case Nil => "No local files left to sync.".fail
        case x if enoughTimePast(lastTimestamp, shippingIntervalInSeconds) =>
          x.success
        case x: List[_]  =>
          (x.size.toString +
            " new logs exist but I synced not long ago.").fail
      }
    case _ => localFiles.success  //All files are left to be synced
  }

  def concatCandidates(candidates: List[File], targetDir: Dir)
  : Validation[String, File] = validate ({
    val combinedName = candidates.last.getName
    val combinedLocalFile = new File(targetDir, combinedName)
    info("Combining " + candidates.size + " into " + combinedLocalFile)
    FileCombiner.combineIntoSeqFile(candidates , combinedLocalFile)
    combinedLocalFile.success
  } , "Can't combine files into a sequence file." +
      " Candidates to combine: " + candidates)

  //I mean almost-atomic!
  def atomicRenameOnHdfs(fs: HdfsFileSystem, src: HdfsFile, dest: HdfsDir)
  : Validation[String, HdfsFile]
  = validate({
    info("Moving " + src + " to " + dest + " @ "  + fs.getUri)
    fs.rename(src, dest) match {
      case true => new HdfsFile(dest, src.getName).success
      case false => ("Rename " + src + " to " + dest + " failed.").fail
    }}, "Rename failed due to IO error")

  def shipToHdfs(fs: HdfsFileSystem, localFile: File, hdfsDir: HdfsDir)
  : Validation[String, HdfsFile] = validate ({
    info("Shipping " + localFile + " to " + hdfsDir + " @ " + fs.getUri)
    fs.copyFromLocalFile(true, true, localFile.getPath, hdfsDir)
    hdfsDir.suffix("/" + localFile.getName).success
  }, "Can't ship to hdfs from " + localFile + " to " + hdfsDir )

  def listHdfsFiles(fs: HdfsFileSystem, hdfsDir: HdfsDir,
                    exclude: List[String] = List.empty)
  : Validation[String, List[HdfsFile]]
  = validate(
      fs.listStatus(hdfsDir)
        .toList
        .map(_.getPath)
        .filterNot(x => exclude.contains(x.getName))
        .success
   , "Can't get list of files on HDFS dir: " + hdfsDir)

  def listLocalFiles(serviceDir : Dir, exclude: List[String] = List.empty)
  : Validation[String, List[File]] = validate (
    serviceDir.listFiles
               .toList
               .filterNot(x => exclude.contains(x.getName))
               .success
  , "Can't list local files on: " + serviceDir)

  def ensureHdfsDir(fs: HdfsFileSystem, hdfsDir: HdfsDir)
  : Validation[String, HdfsDir] = validate(
    tap(hdfsDir)(HDFS.createPath(fs, _)).success
  , "Can't ensure/create dirs on hdfs:"  + hdfsDir)

  def enoughTimePast(base: DateTime, enoughSeconds: Int)
  : Boolean = !(new Interval(base, DateTime.now(DateTimeZone.UTC))
    .toDuration.toStandardSeconds isLessThan Seconds.seconds(enoughSeconds))

  def loadConf(args: Array[String]) : (Configuration, String, String) = {
    val (remainingArgs, conf) = HDFS.parseHadoopConf(args)
    val rootLogDir = remainingArgs(0)
    val rootHdfsDir = remainingArgs(1)
    (conf, rootLogDir, rootHdfsDir)
  }

  implicit def string2File(str: String) : File = new File(str)
  implicit def string2HdfsFile(str: String) : HdfsFile = new HdfsFile(str)
  def tap[A](a: A)(f: A => Unit) : A = {f(a); a}

}

object Logging {

  //private val (logger, formatter) = ZeroLoggerFactory.newLogger(this)

  //import formatter._

  def info(s: String) = println(s)//logger.info(s)
  def warn(s: String) = println(s)//logger.warning(s)
  def error(s: String) = println(s)//logger.severe(s)
}


