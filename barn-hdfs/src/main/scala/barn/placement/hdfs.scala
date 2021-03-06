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

  implicit def dateBucket2Date(d: DateBucket) : DateTime
  = new DateTime(d.year, d.month, d.day, 0, 0)

  def cachingLs(fs: LazyHdfsFileSystem
              , path: HdfsDir
              , hdfsListCache: HdfsListCache)
  : \/[barn.BarnError, \/[BarnError, List[PlacedFileInfo]]] = {
    hdfsListCache.get(path) match {
      case Some(x) => x
      case None => {
        val listResult = listHdfsFiles(fs, path).map(x => collapseValidate(x map getPlacedFileInfo))
        hdfsListCache += ( path -> listResult)
        info(s"I called LS on $path and have a cache of size " + hdfsListCache.size)
        listResult
      }
    }
  }

  def planNextShip(fs: LazyHdfsFileSystem
                 , serviceInfo: LocalServiceInfo
                 , totalReadySize: Long
                 , maxReadySize: Long
                 , baseHdfsDir: HdfsDir
                 , shipInterval: Int
                 , lookBackLowerBound: DateTime
                 , hdfsListCache: HdfsListCache)
  : BarnError \/ ShippingPlan = {

    val hdfsTempDir = targetTempDir(baseHdfsDir)
    val hdfsDirStream = targetDirs(DateTime.now, baseHdfsDir, lookBackLowerBound)

    val targetHdfsDir = hdfsDirStream.head

    val dirsWithRelevantHdfsFilesStream = for {
        each           <- hdfsDirStream
        relevantFiles  <- cachingLs(fs, each , hdfsListCache) match {
			      case -\/(FileNotFound(_)) => None
			      case -\/(a) => -\/(a) some
            case \/-(-\/(a)) => -\/(a) some
			      case \/-(\/-(Nil)) => None
			      case \/-(a) =>
				logsForService(serviceInfo, a) match {
				  case \/-(Nil) => None
				  case otherwise => otherwise some
				}
			  }
      } yield relevantFiles

    for {
       hdfsFilesFileInfo    <- dirsWithRelevantHdfsFilesStream
                                .headOption
                                .getOrElse(List.empty[PlacedFileInfo].right)

       lastShippedTaistamp   = getLastShippedTaistamp(hdfsFilesFileInfo, serviceInfo)
       lastShippedTimestamp  = lastShippedTaistamp.map(Tai64.convertTai64ToTime(_))

       _                    <- isShippingTime(lastShippedTimestamp
                                            , shipInterval
                                            , totalReadySize
                                            , maxReadySize)

    } yield ShippingPlan(targetHdfsDir, hdfsTempDir, lastShippedTaistamp)

  }

  def logsForService(serviceInfo: LocalServiceInfo,
                     hdfsFilesPlacedInfos: BarnError \/ List[PlacedFileInfo])
  : BarnError \/ List[PlacedFileInfo] = for {
    hdfsFilesPlacedInfos_ <- hdfsFilesPlacedInfos
    hdfsFilesFiltered      = filter(hdfsFilesPlacedInfos_, serviceInfo)
  } yield hdfsFilesFiltered

  def getLastShippedTaistamp(hdfsFiles: List[PlacedFileInfo], serviceInfo: LocalServiceInfo)
  : Option[String] = sorted(hdfsFiles, serviceInfo).lastOption.map(_.taistamp)

  def isShippingTime(lastShippedTimestamp: Option[DateTime], shippingInterval: Int, totalReadySize: Long, maxReadySize: Long)
  : BarnError \/ Unit
  = lastShippedTimestamp match {
    case Some(lastTimestamp) =>
      (enoughTimePast(lastTimestamp, shippingInterval) ||
       totalReadySize > maxReadySize) match {
        case true => () right
        case false => SyncThrottled("I synced not long ago.") left
      }
    case None => ().right
  }

  def targetName(taistamp : String, serviceInfo: LocalServiceInfo) :String = {
    val time = Tai64.convertTai64ToTime(taistamp)
    val year = time.getYear
    val month = time.getMonthOfYear
    val date = time.getDayOfMonth
    val pattern = "%04d,%02d,%02d,%s,%s,%s.seq"
    pattern.format(year
                 , month
                 , date
                 , serviceInfo.serviceName
                 , serviceInfo.hostName
                 , taistamp)
  }

  def datePath(baseHdfsDir: HdfsDir, db: DateBucket)
  : HdfsDir = {
    val pattern = "%04d/%02d-%02d"
    new HdfsDir(baseHdfsDir, pattern.format(db.year, db.month, db.day))
  }

  def dateBucket(base: DateTime, daysBefore : Int = 0) : DateBucket = {
    val date = base.minusDays(daysBefore)
    DateBucket(date.getYear, date.getMonthOfYear, date.getDayOfMonth)
  }

  def dateStream(base: DateTime, startingDaysBefore: Int = 0)
  : Stream[DateBucket] = {

    def stream(base: DateTime, b: Int) : Stream[DateBucket] =
      dateBucket(base, b) #:: stream(base, b + 1)

    stream(base, startingDaysBefore)
  }

  private val hdfsFileMatcher =
    "([0-9]{4}),([0-9]{2}),([0-9]{2}),(.*),(.*),(.*).seq".r

  def getPlacedFileInfo(hdfsFile: HdfsFile)
  : BarnError \/ PlacedFileInfo
  = validate(
    hdfsFile.getName match {
      case hdfsFileMatcher(year, month, day, service, host, taistamp) =>
        PlacedFileInfo(DateBucket(year.toInt, month.toInt, day.toInt)
                                , host, service, taistamp) right
      case _ => InvalidNameFormat("Invalid HdfsFile name format " + hdfsFile) left
    }, "Invalid HdfsFile name format " + hdfsFile)

  def targetDirs(base: DateTime, baseHdfsDir: HdfsDir, lowerBound: DateTime)
  : Stream[HdfsDir] = {
    (dateBucket(base, 0) #::
     dateStream(base, 1).takeWhile(_ isAfter lowerBound.minusDays(1).toDateMidnight))
    .map(datePath(baseHdfsDir, _))
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

