package barn

object PlacementGenerators extends PlacementGenerators

trait PlacementGenerators extends CommonGenerators {

  import org.scalacheck.Gen._
  import org.scalacheck.Gen
  import placement.LocalServiceInfo

  val genLocalServiceInfo = for {
    serviceName <- genNonEmptyAlphaNumStr
    hostName <- genHostName
  } yield LocalServiceInfo(serviceName, hostName)

  val genShippingInterval = chooseNum(0, 172800)   //maximum 48 hours

}
