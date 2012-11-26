package barn.placement

import org.joda.time._
import scalaz._
import Scalaz._
import scala.math.Ordering
import barn._
import org.apache.hadoop.fs.{FileSystem => HdfsFileSystem}
import TimeUtils._

object HdfsPlacementStrategy extends HdfsPlacementStrategy

trait HdfsPlacementStrategy
  extends Logging
  with Hadoop
  with SvlogdFile {

  val maxLookBack = 10 //days

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
  : Validation[BarnError, ShippingPlan] = {

    val hdfsTempDir = targetTempDir(baseHdfsDir)
    val hdfsDirStream = targetDirs(baseHdfsDir, maxLookBack)
    val hdfsDir = hdfsDirStream.head

    for {
       _                    <- ensureHdfsDir(fs, hdfsDir)
       _                    <- ensureHdfsDir(fs, hdfsTempDir)
       hdfsFiles            <- listFirstNonEmptyDir(fs, hdfsDirStream)
       lastShippedTaistamp  <- getLastShippedTaistamp(hdfsFiles, serviceInfo)
       lastShippedTimestamp  = lastShippedTaistamp.map(Tai64.convertTai64ToTime(_))
       _                    <- isShippingTime(lastShippedTimestamp, shipInterval)
    } yield ShippingPlan(hdfsDir, hdfsTempDir, lastShippedTaistamp)
  }

  def getLastShippedTaistamp(hdfsFiles: List[HdfsFile], serviceInfo: LocalServiceInfo)
  : Validation[BarnError, Option[String]]
  = for {
      hdfsFilesPlaceInfo <- collapseValidate(hdfsFiles.map(getPlacedFileInfo(_)))
      hdfsFilesFiltered   = filter(hdfsFilesPlaceInfo, serviceInfo)
      hdfsFilesSorted     = sorted(hdfsFilesFiltered, serviceInfo)
      lastShippedFile     = hdfsFilesSorted.lastOption
   } yield lastShippedFile map(_.taistamp)

  def isShippingTime(lastShippedTimestamp: Option[DateTime], shippingInterval: Int)
  : Validation[BarnError, Unit]
  = lastShippedTimestamp match {
    case Some(lastTimestamp) =>
      enoughTimePast(lastTimestamp, shippingInterval) match {
        case true => () success
        case false => SyncThrottled("I synced not long ago.") fail
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
      dateBucket(b) #:: stream(b + 1)

    stream(startingDaysBefore)
  }

  private val hdfsFileMatcher =
    "([0-9]{4}),([0-9]{2}),([0-9]{2}),(.*),(.*),(.*?),(.*).seq".r

  def getPlacedFileInfo(hdfsFile: HdfsFile)
  : Validation[BarnError, PlacedFileInfo]
  = validate(
    hdfsFile.getName match {
      case hdfsFileMatcher(year, month, day, host, service, category, taistamp) =>
        PlacedFileInfo(DateBucket(year.toInt, month.toInt, day.toInt)
                                , host, service, category, taistamp) success
      case _ => InvalidNameFormat("Invalid HdfsFile name format " + hdfsFile) fail
    }, "Invalid HdfsFile name format " + hdfsFile)

  def targetDirs(baseHdfsDir: HdfsDir, maxLookBack: Int)
  : Stream[HdfsDir] = {
    dateStream(0).take(maxLookBack).map(datePath(baseHdfsDir, _))
  }

  def targetTempDir(baseHdfsDir: HdfsDir)
  : HdfsDir
  = new HdfsDir(baseHdfsDir, "tmp/")

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

