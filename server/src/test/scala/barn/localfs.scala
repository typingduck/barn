package barn

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import BarnSuiteGenerators._
import FileTestHelpers._

class LocalFSSuite extends FunSuite with Checkers with barn.LocalFS {

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
          listSortedLocalFiles(base).fold(x => false
          , _.sorted == files.sorted)
        }
      }
    )
  }

}
