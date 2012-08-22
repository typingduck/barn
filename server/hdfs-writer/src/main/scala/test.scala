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
      val remotePath = baseHdfsPath + servicePath.getName
      val remoteTemp = baseHdfsPath + servicePath.getName + "/tmp/"

      val result = for {
        fs          <- Hdfsfsfsfs.createFileSystem(conf)
        localFiles  <- localFileNames(servicePath)
        _           <- ensurePaths(fs, remotePath)
        remoteFiles <- getRemoteFiles(fs, remotePath)
        candidates  <- outstandingFiles(localFiles, remoteFiles)
        concatted   <- concatCandidates(servicePath, candidates)
        hdfsTemp    <- shipToHdfs(fs, concatted, remoteTemp)
        ♬           <- atomicRenameOnHdfs(fs, hdfsTemp, remotePath)
      } yield ♬         // ♬ = tada! -- very limited in scope, hence the name.

      result ||| error

    }, error _)
  }
}

object Steps {

  import java.io.File
  import scala.util.control.Exception._
  import org.joda.time._
  import org.apache.commons.lang.exception.ExceptionUtils._
  import org.apache.hadoop.fs.{Path, FileSystem}
  import org.apache.hadoop.conf.Configuration

  import scalaz._
  import Scalaz._

  import Logging._

  val lineDelim = System.getProperty("line.separator")

  def validate_[U](body: => Validation[String,U], detail: String = null)
  : Validation[String, U]
  = allCatch either body fold ( exception =>
    detail match {
      case null => getStackTrace(exception).fail
      case sth  => (detail + lineDelim + getStackTrace(exception)).fail
    } , identity)

  def validate[U](body: => U, detail: String = null)
  : Validation[String, U]
  = allCatch either body fold ( exception =>
    detail match {
      case null => getStackTrace(exception).fail
      case sth  => (detail + lineDelim + getStackTrace(exception)).fail
    } , _.success)

  def listSubdirectories(path: File)
  : Validation[String, List[File]]
  = validate(path.listFiles.toList.filter(x => x.isDirectory),
    "Can't get list of services directories on: " + path)

  def outstandingFiles(localFiles: List[String], remoteFiles: List[String])
  : Validation[String, List[String]]
  = remoteFiles.sorted.lastOption match {
    case Some(last) =>
      val lastTimestamp = Tai64.convertTai64ToTime(last.drop(1))
      localFiles.sorted.dropWhile(_ <= last) match {
        case Nil => "No local files left to sync.".fail
        case x if timeToFlush(lastTimestamp, 5) => x.success
        case x: List[_]  =>
          (x.size.toString + " new logs exist but I synced not long ago.").fail
      }
    case _ => localFiles.success
  }

  def concatCandidates(localPath: File, candidateNames: List[String])
  : Validation[String, File] = validate ({
    info("Combining " + candidateNames.size + " files from " + localPath)
    val combinedName = candidateNames.last
    val combinedLocalPath = new File("/tmp/" + combinedName)
    val candidatesWithPath = candidateNames.map(new File(localPath, _))
    FileCombiner.combineIntoSeqFile(candidatesWithPath , combinedLocalPath)
    combinedLocalPath
  } , "Can't combine files into a sequence file. local path:" + localPath +
      " candidates to combine: " + candidateNames)

  //I mean almost-atomic!
  def atomicRenameOnHdfs(fs: FileSystem, src: Path, dest: Path)
  : Validation[String, Null]
  = validate_({
    info("Renaming " + src + " to " + dest + " @ "  + fs.getUri)
    fs.rename(src, dest) match {
      case true => Success(null)
      case false => ("Rename " + src + " to " + dest + " failed.").fail
    }}, "Rename failed due to IO error")

  def shipToHdfs(fs: FileSystem, localFile: File, remotePath: Path)
  : Validation[String, Path] = validate ({
    info("Shipping " + localFile + " to " + remotePath + " @ " + fs.getUri)
    fs.copyFromLocalFile(true, true, new Path(localFile.getPath), remotePath)
    remotePath.suffix("/" + localFile.getName)
  }, "Can't ship to hdfs from " + localFile + " to " + remotePath )

  def getRemoteFiles(fs: FileSystem, remotePath: Path)
  : Validation[String, List[String]]
  = validate(
      fs.listStatus(remotePath).toList.map(_.getPath.getName)
        .filterNot(_ == "tmp")
   , "Can't get list of files on HDFS path: " + remotePath)

   def ensurePaths(fs: FileSystem, remotePath: Path)
   : Validation[String, Null] = validate({
     Hdfsfsfsfs.createPath(fs, remotePath)
     Hdfsfsfsfs.createPath(fs, remotePath.suffix("/tmp"))
     null
   }, "Can't ensure/create paths on hdfs: + remotePath")

  def localFileNames(servicePath : File)
  : Validation[String, List[String]] = validate ({
    val exclude = List(".gitignore", "target", "lock", "current")  //Dev, remove me
    servicePath.listFiles.toList.map(_.getName).filterNot(x => exclude.contains(x))
  }, "Can't list local files on: " + servicePath)

  implicit def string2File(path: String) : File = new File(path)
  implicit def string2Path(path: String) : Path = new Path(path)

  def timeToFlush(lastWrittenDate: DateTime, secondsToWait: Int)
  : Boolean = !(new Interval(lastWrittenDate, DateTime.now(DateTimeZone.UTC))
    .toDuration.toStandardSeconds isLessThan Seconds.seconds(secondsToWait))

  def loadConf(args: Array[String]) : (Configuration, String, String) = {
    val (remainingArgs, conf) = Hdfsfsfsfs.parseHadoopConf(args)
    val rootLogPath = remainingArgs(0)
    (conf, rootLogPath, "/user/omid/barn-test/")
  }

}

object Logging {

  //private val (logger, formatter) = ZeroLoggerFactory.newLogger(this)

  //import formatter._

  def info(s: String) = println(s)//logger.info(s)
  def warn(s: String) = println(s)//logger.warning(s)
  def error(s: String) = println(s)//logger.severe(s)
}


