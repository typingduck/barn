package barn

object CommonGenerators extends CommonGenerators

trait CommonGenerators {

  import org.scalacheck.Gen._
  import org.scalacheck.Gen

  val genNonEmptyAlphaNumStr = for {
    alnum <- listOf1(alphaNumChar)
  } yield alnum.mkString.toLowerCase

}
