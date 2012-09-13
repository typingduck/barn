package barn

object BarnSteps {

  import java.io.File
  import sun.misc.BASE64Decoder;
  import scala.util.control.Exception._
  import scala.util.matching.Regex;
  import org.joda.time._
  import org.apache.commons.lang.exception.ExceptionUtils._
  import org.apache.commons.lang.RandomStringUtils
  import org.apache.hadoop.fs.{Path => HdfsFile, FileSystem => HdfsFileSystem}
  import org.apache.hadoop.conf.Configuration

  import scalaz._
  import Scalaz._

  import Logging._
  import scala.math.Ordering

  import Utils._

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
                   minMB: Int,
                   exclude: List[String] = List.empty)
  : Validation[String, Unit]
  = for {
      localFiles <- listLocalFiles(dir, exclude)
      numDeleted <- validate({
        val sumSize = localFiles.foldLeft(0L) { (sum, f) => sum + f.length}
        localFiles.foldLeft((0, sumSize)) {
          case deletedSoFar -> curSize -> file =>
            val ts = Tai64.convertTai64ToTime(stripSvlogdPrefix(file.getName))

            enoughTimePast(ts, localRetention) &&
              ts.isBefore(cleanupLimit) &&
              curSize > minMB * 1024 * 1024  match {
              case true =>
                val fileLength = file.length
                file.delete
                (deletedSoFar + 1) -> (curSize - fileLength)
              case false => deletedSoFar -> curSize
            }
        }
      }.success, "Deletion of retained files failed.")
    } yield tap(numDeleted) (x => info (x._1 + " retained files deleted and " +
                                        x._2 / (1024 * 1024) + "MB remained."))

  def listSubdirectories(dir: Dir)
  : Validation[String, List[Dir]]
  = validate(dir.listFiles.toList.filter(x => x.isDirectory).success,
    "Can't get list of local services directories on: " + dir)

  implicit object HdfsFileOrderingByName extends Ordering[HdfsFile] {
    def compare(o1: HdfsFile, o2: HdfsFile) = o1.getName compare o2.getName
  }

  def stripSvlogdPrefix(svlogdFileName: String)
  : String = svlogdFileName.drop(1)

  def earliestTimestamp(localFiles: List[File])
  : Validation[String, DateTime]
  = Tai64.
      convertTai64ToTime(stripSvlogdPrefix(localFiles.head.getName)).success

  def isShippingTime(hdfsFiles: List[HdfsFile],
                   shippingIntervalInSeconds: Int,
                   tsConvert: HdfsFile => DateTime)
  : Validation[String, Unit]
  = hdfsFiles.sorted.lastOption match {
    case Some(last) =>
      val lastTimestamp = tsConvert(last)
      enoughTimePast(lastTimestamp, shippingIntervalInSeconds) match {
        case true => ().success
        case false => ("I synced not long ago.").fail
      }
    case None => ().success
  }

  def outstandingFiles(localFiles: List[File],
                       hdfsFiles: List[HdfsFile],
                       getTaiStamp: HdfsFile => String)
  : Validation[String, List[File]]
  = hdfsFiles.lastOption match {
    case Some(last) =>
      localFiles.sorted.dropWhile(f =>
        stripSvlogdPrefix(f.getName) <= getTaiStamp(last)) match {
        case Nil => "No local files left to sync.".fail
        case x => x.success
      }
    case _ => localFiles.success  //All files are left to be synced
  }

  val BASE64 = "base64"
  val syslogMatcher = "(?:^.*?\\[origin.*?x-encoding\\s*?=\\s*?\"\\s*(.*?)\\s*\".*?\\]\\s)?(.*)".r
  val base64Decoder = new BASE64Decoder();

  def processFormat(line: String) : Array[Byte]
  = syslogMatcher.findFirstMatchIn(line) match {
      case Some(syslogMatcher(BASE64,body)) => base64Decoder.decodeBuffer(body)
      case Some(_) => line.getBytes
      case None => throw new java.lang.Exception("Regexp match to extract body failed?? Come save me!") //FIXME
    }

  def concatCandidates(candidates: List[File], targetDir: Dir)
  : Validation[String, File] = validate ({
    val combinedName = RandomStringUtils.randomAlphanumeric(20)
    val combinedLocalFile = new File(targetDir, combinedName)
    info("Combining " + candidates.size + " into " + combinedLocalFile)
    FileCombiner.combineIntoSeqFile(candidates, combinedLocalFile, processFormat)
    combinedLocalFile.success
  } , "Can't combine files into a sequence file." +
      " Candidates to combine: " + candidates)

  //I mean almost-atomic!
  def atomicRenameOnHdfs(fs: HdfsFileSystem, src: HdfsFile, dest: HdfsDir,
                         newName: String)
  : Validation[String, HdfsFile]
  = validate({
    val targetHdfsFile = new HdfsFile(dest, newName)
    info("Moving " + src + " to " + targetHdfsFile + " @ "  + fs.getUri)
    fs.rename(src, targetHdfsFile) match {
      case true => targetHdfsFile.success
      case false => ("Rename " + src + " to " + dest + " failed.").fail
    }}, "Rename failed due to IO error")

  def shipToHdfs(fs: HdfsFileSystem, localFile: File, hdfsDir: HdfsDir)
  : Validation[String, HdfsFile] = validate ({
    info("Shipping " + localFile + " to " + hdfsDir + " @ " + fs.getUri)
    fs.copyFromLocalFile(true, true, new HdfsFile(localFile.getPath), hdfsDir)
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
               .filterNot(x => exclude.foldLeft(false) {
                  (res, pattern) => res || x.getName.matches(pattern) })
               .success
  , "Can't list local files on: " + serviceDir)

  def ensureHdfsDir(fs: HdfsFileSystem, hdfsDir: HdfsDir)
  : Validation[String, HdfsDir] = validate(
    tap(hdfsDir)(HDFS.createPath(fs, _)).success
  , "Can't ensure/create dirs on hdfs:"  + hdfsDir)

  def enoughTimePast(base: DateTime, enoughSeconds: Int)
  : Boolean = !(new Interval(base, DateTime.now(DateTimeZone.UTC))
    .toDuration.toStandardSeconds isLessThan Seconds.seconds(enoughSeconds))

  def loadConf(args: Array[String]) : (Configuration, Dir, HdfsDir) = {
    val (remainingArgs, conf) = HDFS.parseHadoopConf(args)
    val rootLogDir = new Dir(remainingArgs(0))
    val rootHdfsDir = new HdfsDir(remainingArgs(1))
    (conf, rootLogDir, rootHdfsDir)
  }
}


