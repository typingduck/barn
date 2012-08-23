package barn

object BarnHdfsWriter extends App {

  import Stream.continually
  import scalaz._
  import Scalaz._
  import Logging._
  import Steps._

  val (conf, rootLogPath, baseHdfsPath) = loadConf(args)

  continually(() => listSubdirectories(rootLogPath)).iterator.foreach {
    _().toOption.fold( _.foreach { servicePath =>
      info("Checking service " + servicePath + " to sync.")

      val hdfsPath = baseHdfsPath + servicePath.getName
      val hdfsTempPath = baseHdfsPath + servicePath.getName + "/tmp/"

      val excludeLocal = List(".gitignore", "target", "lock", "current")

      val result = for {
        fs          <- HDFS.createFileSystem(conf)
        localFiles  <- localFiles(servicePath, excludeLocal)
        _           <- ensurePaths(fs, hdfsPath)
        hdfsFiles   <- getRemoteFiles(fs, hdfsPath)
        candidates  <- outstandingFiles(localFiles, hdfsFiles)
        concatted   <- concatCandidates(candidates)
        hdfsTemp    <- shipToHdfs(fs, concatted, hdfsTempPath)
        ♬           <- atomicRenameOnHdfs(fs, hdfsTemp, hdfsPath)
      } yield ♬     // ♬ = tada! -- very limited in scope, hence the name.

      result ||| error

    }, error _)
  }
}

object Steps {

  import java.io.File
  import scala.util.control.Exception._
  import org.joda.time._
  import org.apache.commons.lang.exception.ExceptionUtils._
  import org.apache.hadoop.fs.{Path => HdfsPath, FileSystem => HdfsFileSystem}
  import org.apache.hadoop.conf.Configuration

  import scalaz._
  import Scalaz._

  import Logging._
  import scala.math.Ordering

  val lineDelim = System.getProperty("line.separator")

  def validate[U](body: => Validation[String,U], detail: String = null)
  : Validation[String, U]
  = allCatch either body fold ( exception =>
    detail match {
      case null => getStackTrace(exception).fail
      case sth  => (detail + lineDelim + getStackTrace(exception)).fail
    } , identity)

  def listSubdirectories(path: File)
  : Validation[String, List[File]]
  = validate(path.listFiles.toList.filter(x => x.isDirectory).success,
    "Can't get list of local services directories on: " + path)

  implicit object HdfsPathOrderingByName extends Ordering[HdfsPath] {
    def compare(o1: HdfsPath, o2: HdfsPath) = o1.getName compare o2.getName
  }

  def outstandingFiles(localFiles: List[File], hdfsFiles: List[HdfsPath])
  : Validation[String, List[File]]
  = hdfsFiles.sorted.lastOption match {
    case Some(last) =>
      val lastTimestamp = Tai64.convertTai64ToTime(last.getName.drop(1))
      localFiles.sorted.dropWhile(_.getName <= last.getName) match {
        case Nil => "No local files left to sync.".fail
        case x if timeToFlush(lastTimestamp, 5) => x.success
        case x: List[_]  =>
          (x.size.toString + " new logs exist but I synced not long ago.").fail
      }
    case _ => localFiles.success  //All files are left to be synced
  }

  def concatCandidates(candidates: List[File],
                       tempFolder: File = "/tmp")
  : Validation[String, File] = validate ({
    val combinedName = candidates.last.getName
    val combinedLocalPath = new File(tempFolder, combinedName)
    info("Combining " + candidates.size + " into " + combinedLocalPath)
    FileCombiner.combineIntoSeqFile(candidates , combinedLocalPath)
    combinedLocalPath.success
  } , "Can't combine files into a sequence file." +
      " Candidates to combine: " + candidates)

  //I mean almost-atomic!
  def atomicRenameOnHdfs(fs: HdfsFileSystem, src: HdfsPath, dest: HdfsPath)
  : Validation[String, Null]
  = validate({
    info("Moving " + src + " to " + dest + " @ "  + fs.getUri)
    fs.rename(src, dest) match {
      case true => Success(null)
      case false => ("Rename " + src + " to " + dest + " failed.").fail
    }}, "Rename failed due to IO error")

  def shipToHdfs(fs: HdfsFileSystem, localFile: File, hdfsPath: HdfsPath)
  : Validation[String, HdfsPath] = validate ({
    info("Shipping " + localFile + " to " + hdfsPath + " @ " + fs.getUri)
    fs.copyFromLocalFile(true, true, localFile.getPath, hdfsPath)
    hdfsPath.suffix("/" + localFile.getName).success
  }, "Can't ship to hdfs from " + localFile + " to " + hdfsPath )

  def getRemoteFiles(fs: HdfsFileSystem, hdfsPath: HdfsPath)
  : Validation[String, List[HdfsPath]]
  = validate(
      fs.listStatus(hdfsPath)
        .toList
        .map(_.getPath)
        .filterNot(_.getName == "tmp")
        .success
   , "Can't get list of files on HDFS path: " + hdfsPath)

   def ensurePaths(fs: HdfsFileSystem, hdfsPath: HdfsPath)
   : Validation[String, Null] = validate({
     HDFS.createPath(fs, hdfsPath)
     HDFS.createPath(fs, hdfsPath.suffix("/tmp"))
     Success(null)
   }, "Can't ensure/create paths on hdfs: + hdfsPath")

  def localFiles(servicePath : File, exclude: List[String] = List.empty)
  : Validation[String, List[File]] = validate ({
    servicePath.listFiles
               .toList
               .filterNot(x => exclude.contains(x.getName))
               .success
  }, "Can't list local files on: " + servicePath)

  def timeToFlush(lastWrittenDate: DateTime, secondsToWait: Int)
  : Boolean = !(new Interval(lastWrittenDate, DateTime.now(DateTimeZone.UTC))
    .toDuration.toStandardSeconds isLessThan Seconds.seconds(secondsToWait))

  def loadConf(args: Array[String]) : (Configuration, String, String) = {
    val (remainingArgs, conf) = HDFS.parseHadoopConf(args)
    val rootLogPath = remainingArgs(0)
    (conf, rootLogPath, "/user/omid/barn-test/")
  }

  implicit def string2File(path: String) : File = new File(path)
  implicit def string2Path(path: String) : HdfsPath = new HdfsPath(path)
}

object Logging {

  //private val (logger, formatter) = ZeroLoggerFactory.newLogger(this)

  //import formatter._

  def info(s: String) = println(s)//logger.info(s)
  def warn(s: String) = println(s)//logger.warning(s)
  def error(s: String) = println(s)//logger.severe(s)
}


