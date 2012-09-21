package barn

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers

class PlacementSuite extends FunSuite with Checkers {

  import HdfsPlacementStrategy._
  import scalaz._
  import Scalaz._

  import org.scalacheck.Arbitrary._
  import org.scalacheck.Prop._

  import BarnSuiteGenerators._
  import FileTestHelpers._



}
