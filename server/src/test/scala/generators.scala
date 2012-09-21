package barn

object BarnSuiteGenerators {

  import org.scalacheck.Gen._
  import org.scalacheck.Gen
  import org.joda.time._
  import Tai64._
  import Types._

  val systemTemp = new Dir(System.getProperty("java.io.tmpdir"))
  val hdfsTemp = new HdfsDir("/tmp/")

  val genNonEmptyAlphaNumStr = for {
    alnum <- listOf1(alphaNumChar)
  } yield alnum.mkString.toLowerCase

  def genDirectory(base: Dir) = for {
    dir <- genNonEmptyAlphaNumStr
  } yield new File(base, dir)

  def genFile(base: Dir) = for {
    name <- genNonEmptyAlphaNumStr
  } yield new File(base, name)

  def genFiles(base: Dir) = for {
    files <- listOf1(genFile(base))
  } yield files

  def genHdfsFile(base: HdfsDir, nameGen: Gen[String]) = for {
    name <- genNonEmptyAlphaNumStr
  } yield new HdfsFile(base, name)

  def genHdfsFiles(base: HdfsDir, nameGen: Gen[String]) = for {
    hdfsFiles <- listOf1(genHdfsFile(base, nameGen))
  } yield hdfsFiles

  val genDirStructure = for {
    base <- genDirectory(systemTemp)
    subs <- listOf1(genDirectory(base))
  } yield (base, subs.distinct)

  val genDirStructureWithFiles = for {
    (base, subs) <- genDirStructure
    files <- genFiles(base)
  } yield (base, subs, (files -- subs).distinct)

  val baseDate = new DateTime(1970, 1, 1, 0, 0, 0, 0, DateTimeZone.UTC);

  def genDate(after: DateTime = baseDate) = for {
    second <- choose(1, 60 * 365 * 24 * 60 * 60)
  } yield baseDate.plusSeconds(second)

  def genTai64Date(after: DateTime = baseDate) = for {
    date <- genDate(after)
  } yield convertDateToTai64(date)

  val genSvlogdFileName = for {
    date <- genTai64Date(baseDate)
  } yield "@" + date + ".s"

}
