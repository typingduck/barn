package barn

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
  : Validation[String, HdfsDir]
  = new HdfsDir(baseHdfsDir, datePath(DateTime.now)).success

  def datePath(date: DateTime)
  : HdfsDir
  = new HdfsDir("/" + date.getYear
              + "/" + date.getMonthOfYear
              + "-" + date.getDayOfMonth)

  def targetTempDir(baseHdfsDir: HdfsDir, serviceInfo: ServiceInfo)
  : Validation[String, HdfsDir] = new HdfsDir(baseHdfsDir, "tmp/").success

  def decodeServiceInfo(serviceDir: Dir)
  : Validation[String, ServiceInfo]
  = serviceDir.getName.split(delim) match {
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


