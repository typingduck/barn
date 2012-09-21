package barn

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers

class BarnStepsSuite extends FunSuite with Checkers {

  import BarnSteps._
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

  test("Listing subdirectories should only include directories") {
    check(
      forAll(genDirStructureWithFiles) { case (base, subs, files) =>
        havingTemporaryDirStructure(base, subs, files) {
          listSubdirectories(base).fold(_ => false, _.sorted == subs.sorted)
        }
      }
    )
  }

  test("Should list local files and respect exclusions") {
    check(
      forAll(genDirStructureWithFiles) { case (base, subs, files) =>
        havingTemporaryDirStructure(base, subs, files) {
          listLocalFiles(base).fold(x => false
          , _.sorted == files.sorted)
        }
      }
    )
  }

  val tai64Length = 24

  test("Should strip svlogd file names' prefix properly") {
    check(
      forAll(genSvlogdFileName) { fileName =>
        val stripped = svlogdFileNameToTaiString(fileName)
        (stripped.size == tai64Length) :| "Invalid Tai64 timestamp"
      }
    )
  }

  test("Tai64 timestamp conversion should be one-to-one") {
    check(
      forAll(genDate()) { date =>
        Tai64.convertTai64ToTime(Tai64.convertDateToTai64(date)) == date
      }
    )
  }

}
