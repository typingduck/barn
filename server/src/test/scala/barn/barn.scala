package barn

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers

class BarnStepsSuite extends FunSuite with Checkers {

  import scalaz._
  import Scalaz._

  import org.scalacheck.Arbitrary._
  import org.scalacheck.Prop._

  import BarnSuiteGenerators._
  import FileTestHelpers._

  test("Validate should catch all exceptions and return failure.") {
    def f = throw new RuntimeException("hola")
    val error = "function didn't run successfully"
    val result = validate(f, error, false)
    assert(result === Failure(error))
  }

  test("Validate should return success if no exception happened") {
    def f = 1.success
    val error = "function didn't run successfully"
    val result = validate(f, error)
    assert(result === f)
  }

}
