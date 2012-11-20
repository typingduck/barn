package barn.placement

import org.joda.time._
import scalaz._
import Scalaz._
import scala.math.Ordering
import barn._
import org.apache.hadoop.fs.{FileSystem => HdfsFileSystem}
import TimeUtils._

trait HdfsPlacementStrategy
  extends Logging
  with Hadoop
  with SvlogdFile {

  case class DateBucket(year: Int,
                        month: Int,
                        day: Int)

  case class PlacedFileInfo(bucket   : DateBucket
                         , host     : String
                         , service  : String
                         , category : String
                         , taistamp : String)

  def planNextShip(fs: HdfsFileSystem
                 , serviceInfo: LocalServiceInfo
                 , baseHdfsDir: HdfsDir
                 , shipInterval: Int)
  : Validation[String, ShippingPlan] =
  for {
     hdfsDirStream        <- targetDirs(baseHdfsDir)
     hdfsDir               = hdfsDirStream.head
     hdfsTempDir          <- targetTempDir(baseHdfsDir)
     _                    <- ensureHdfsDir(fs, hdfsDir)
     _                    <- ensureHdfsDir(fs, hdfsTempDir)
     hdfsFiles            <- listHdfsFiles(fs, hdfsDir)
     lastShippedTaistamp  <- getLastShippedTaistamp(hdfsFiles, serviceInfo)
     lastShippedTimestamp  = lastShippedTaistamp.map(Tai64.convertTai64ToTime(_))
     _                    <- isShippingTime(lastShippedTimestamp, shipInterval)
  } yield ShippingPlan(hdfsDir, hdfsTempDir, lastShippedTaistamp)

  def getLastShippedTaistamp(hdfsFiles: List[HdfsFile], serviceInfo: LocalServiceInfo)
  : Validation[String, Option[String]]
  = for {
      hdfsFilesPlaceInfo <- collapseValidate(hdfsFiles.map(getPlacedFileInfo(_)))
      hdfsFilesFiltered   = filter(hdfsFilesPlaceInfo, serviceInfo)
      hdfsFilesSorted     = sorted(hdfsFilesFiltered, serviceInfo)
      lastShippedFile     = hdfsFilesSorted.lastOption
   } yield lastShippedFile map(_.taistamp)

  def isShippingTime(lastShippedTimestamp: Option[DateTime], shippingInterval: Int)
  : Validation[String, Unit]
  = lastShippedTimestamp match {
    case Some(lastTimestamp) =>
      enoughTimePast(lastTimestamp, shippingInterval) match {
        case true => () success
        case false => "I synced not long ago." fail
      }
    case None => ().success
  }

  def targetName(taistamp : String, serviceInfo: LocalServiceInfo) :String = {
    val time = Tai64.convertTai64ToTime(taistamp)
    val year = time.getYear
    val month = time.getMonthOfYear
    val date = time.getDayOfMonth
    val pattern = "%04d,%02d,%02d,%s,%s,%s,%s.seq"
    pattern.format(year
                 , month
                 , date
                 , serviceInfo.hostName
                 , serviceInfo.serviceName
                 , serviceInfo.category
                 , taistamp)
  }

  def datePath(baseHdfsDir: HdfsDir, db: DateBucket)
  : HdfsDir = {
    val pattern = "%04d/%02d-%02d"
    new HdfsDir(baseHdfsDir, pattern.format(db.year, db.month, db.day))
  }

  def dateBucket(daysBefore : Int = 0) : DateBucket = {
    val date = DateTime.now.minusDays(daysBefore)
    DateBucket(date.getYear, date.getMonthOfYear, date.getDayOfMonth)
  }

  def dateStream(startingDaysBefore : Int = 0) : Stream[DateBucket] = {
    def stream(b: Int) : Stream[DateBucket] =
      dateBucket(startingDaysBefore) #:: stream(startingDaysBefore + 1)

    stream(0)
  }

  private val hdfsFileMatcher =
    "([0-9]{4}),([0-9]{2}),([0-9]{2}),(.*),(.*),(.*?),(.*).seq".r

  def getPlacedFileInfo(hdfsFile: HdfsFile)
  : Validation[String, PlacedFileInfo]
  = validate(
    hdfsFile.getName match {
      case hdfsFileMatcher(year, month, day, host, service, category, taistamp) =>
        PlacedFileInfo(DateBucket(year.toInt, month.toInt, day.toInt)
                                , host, service, category, taistamp) success
      case _ => ("Invalid HdfsFile name format " + hdfsFile) fail
    }, "Invalid HdfsFile name format " + hdfsFile)

  def targetDirs(baseHdfsDir: HdfsDir)
  : Validation[String, Stream[HdfsDir]] = {
    dateStream(0).map(datePath(baseHdfsDir, _)) success
  }

  def targetTempDir(baseHdfsDir: HdfsDir)
  : Validation[String, HdfsDir]
  = new HdfsDir(baseHdfsDir, "tmp/") success

  implicit object PlacedFileOrderingByTaistamp extends Ordering[PlacedFileInfo] {
    def compare(o1: PlacedFileInfo, o2: PlacedFileInfo) : Int
    = o1.taistamp compare o2.taistamp
  }

  def sorted(list: List[PlacedFileInfo], service: LocalServiceInfo)
  : List[PlacedFileInfo] = list.sorted

  def filter(list: List[PlacedFileInfo], service: LocalServiceInfo)
  : List[PlacedFileInfo]
  = list.filter(matches(_, service))

  def matches(placedFile: PlacedFileInfo, serviceInfo: LocalServiceInfo)
  : Boolean
  = placedFile.service == serviceInfo.serviceName &&
    placedFile.host    == serviceInfo.hostName
}

