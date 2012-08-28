package barn

object BarnHdfsWriter extends App {

  import Stream.continually
  import scalaz._
  import Scalaz._
  import Logging._
  import BarnSteps._

  val (conf, rootLogDir, baseHdfsDir) = loadConf(args)

  val PS = HdfsPlacementStrategy

  val localTempDir = new Dir("/tmp")

  val excludeLocal = List("\\.gitignore", "target", "lock", "current", "\\..*")
  val excludeHdfs = List("tmp")

  val shipInterval = 10 //in seconds
  val retention = 60 //in seconds

  continually(() => listSubdirectories(rootLogDir)).iterator.foreach {
    _().toOption.fold( _.foreach { serviceDir =>
      info("Checking service " + serviceDir + " to sync.")

      val result = for {
        serviceInfo  <- PS.decodeServiceInfo(serviceDir)
        fs           <- HDFS.createFileSystem(conf)

        hdfsDir      <- PS.targetDir(baseHdfsDir, serviceInfo)
        hdfsTempDir  <- PS.targetTempDir(baseHdfsDir, serviceInfo)
        _            <- ensureHdfsDir(fs, hdfsDir)
        _            <- ensureHdfsDir(fs, hdfsTempDir)

        allHdfsFiles <- listHdfsFiles(fs, hdfsDir, excludeHdfs)
        hdfsFiles     = PS.sorted(PS.filter(allHdfsFiles, serviceInfo), serviceInfo)
        _            <- isShippingTime(hdfsFiles, shipInterval, PS.extractTS)

        localFiles   <- listLocalFiles(serviceDir, excludeLocal)
        candidates   <- outstandingFiles(localFiles, hdfsFiles, PS.extractTai)
        concatted    <- concatCandidates(candidates, localTempDir)

        hdfsName     <- PS.targetName(candidates, serviceInfo)
        hdfsTempFile <- shipToHdfs(fs, concatted, hdfsTempDir)
        _            <- atomicRenameOnHdfs(fs, hdfsTempFile, hdfsDir, hdfsName)

        cleanupLimit <- earliestTimestamp(candidates)
        ♬            <- cleanupLocal(serviceDir, retention, cleanupLimit,
                                     excludeLocal)
      } yield ♬     // ♬ = tada! -- very limited in scope, hence the name.

      result ||| error

    }, error _)
  }
}

object HdfsPlacementStrategy { //TODO be a trait to be able to have multiple

  import java.io.File
  import org.apache.hadoop.fs.{Path => HdfsFile}

  import org.joda.time._

  import scalaz._
  import Scalaz._

  import Utils._

  type Dir = File
  type HdfsDir = HdfsFile

  type ServiceName = String
  type HostName = String

  val delim = '@'

  case class ServiceInfo(serviceName: ServiceName, hostName: HostName)

  def extractTai(hdfsFile: HdfsFile) : String
  = hdfsFile.getName.split(delim).last

  def extractTS(hdfsFile: HdfsFile) : DateTime
  = Tai64.convertTai64ToTime(hdfsFile.getName.split("@").last)

  def targetName(candidates: List[File], serviceInfo: ServiceInfo)
  : Validation[String, String]
  = (encodeServiceInfo(serviceInfo) + candidates.last.getName).success

  def targetDir(baseHdfsDir: HdfsDir, serviceInfo: ServiceInfo)
  : Validation[String, HdfsDir] = baseHdfsDir.success

  def targetTempDir(baseHdfsDir: HdfsDir, serviceInfo: ServiceInfo)
  : Validation[String, HdfsDir] = new HdfsDir(baseHdfsDir, "tmp/").success

  def decodeServiceInfo(serviceDir: Dir)
  : Validation[String, ServiceInfo]
  = tap(serviceDir)(println).getName.split(delim) match {
    case Array(serviceName, hostName) =>
      ServiceInfo(serviceName, hostName).success
    case _ => ("Failed to extract service info for " + serviceDir).fail
  }

  def encodeServiceInfo(serviceInfo: ServiceInfo) : String
  = serviceInfo.serviceName + delim + serviceInfo.hostName

  def filter(list: List[HdfsFile], service: ServiceInfo)
  : List[HdfsFile] = list.filter(matches(_, service))

  def sorted(list: List[HdfsFile], service: ServiceInfo)
  : List[HdfsFile] = list.sortWith((a,b) => extractTai(a) < extractTai(b))

  def matches(hdfsFile: HdfsFile, serviceInfo: ServiceInfo) : Boolean
  = hdfsFile.getName.startsWith(encodeServiceInfo(serviceInfo))
}

object BarnSteps {

  import java.io.File
  import scala.util.control.Exception._
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
      localFiles.sorted.dropWhile(_.getName.drop(1) <= getTaiStamp(last)) match {
        case Nil => "No local files left to sync.".fail
        case x => x.success
      }
    case _ => localFiles.success  //All files are left to be synced
  }

  def concatCandidates(candidates: List[File], targetDir: Dir)
  : Validation[String, File] = validate ({
    val combinedName = RandomStringUtils.randomAlphanumeric(20)
    val combinedLocalFile = new File(targetDir, combinedName)
    info("Combining " + candidates.size + " into " + combinedLocalFile)
    FileCombiner.combineIntoSeqFile(candidates , combinedLocalFile)
    combinedLocalFile.success
  } , "Can't combine files into a sequence file." +
      " Candidates to combine: " + candidates)

  //I mean almost-atomic!
  def atomicRenameOnHdfs(fs: HdfsFileSystem, src: HdfsFile, dest: HdfsDir, newName: String)
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

object Logging {

  //private val (logger, formatter) = ZeroLoggerFactory.newLogger(this)

  //import formatter._

  def info(s: String) = println(s)//logger.info(s)
  def warn(s: String) = println(s)//logger.warning(s)
  def error(s: String) = println(s)//logger.severe(s)
}


