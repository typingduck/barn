package barn

object LocalFSGenerators extends CommonGenerators {

  import org.scalacheck.Gen._
  import org.scalacheck.Gen

  val systemTemp = new Dir(System.getProperty("java.io.tmpdir"))

  def genDirectory(base: Dir) = for {
    dir <- genNonEmptyAlphaNumStr
  } yield new File(base, dir)

  def genFile(base: Dir) = for {
    name <- genNonEmptyAlphaNumStr
  } yield new File(base, name)

  def genFiles(base: Dir) = for {
    files <- listOf1(genFile(base))
  } yield files

  val genDirStructure = for {
    base <- genDirectory(systemTemp)
    subs <- listOf1(genDirectory(base))
  } yield (base, subs.distinct)

  val genDirStructureWithFiles = for {
    (base, subs) <- genDirStructure
    files <- genFiles(base)
  } yield (base, subs, (files filterNot (subs contains)).distinct)

}
