package barn

import scalaz._
import Scalaz._

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._
import BarnSuiteGenerators._

class TimeSuite
  extends FunSuite
  with Checkers
  with SvlogdFile {

  test("Should check if enough seconds is passed properly") {
      //TODO
   }

}
