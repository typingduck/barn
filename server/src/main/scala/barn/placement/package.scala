package barn

package object placement {

  case class LocalServiceInfo(serviceName: String, hostName: String, category: String)

  case class ShippingPlan(hdfsDir: HdfsDir
                        , hdfsTempDir: HdfsDir
                        , lastTaistamp: Option[String])

}
