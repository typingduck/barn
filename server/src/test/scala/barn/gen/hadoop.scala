package barn

object HadoopGenerators extends HadoopGenerators

trait HadoopGenerators extends CommonGenerators {

  import org.scalacheck.Gen._
  import org.scalacheck.Gen

  val hdfsTemp = new HdfsDir("/tmp/")

  def genHdfsDirectory(base: HdfsDir) = for {
    dir <- genNonEmptyAlphaNumStr
  } yield new HdfsDir(base, dir)

  def genHdfsFile(base: HdfsDir)
  = for {
    name <- genNonEmptyAlphaNumStr
  } yield new HdfsFile(base, name)

  def genHdfsFiles(base: HdfsDir)
  = for {
    hdfsFiles <- listOf(genHdfsFile(base))
  } yield hdfsFiles

  val genHdfsDirWithFiles = for {
    base  <- genHdfsDirectory(hdfsTemp)
    files <- genHdfsFiles(base)
  } yield (base, files.distinct)

  val genHdfsDirStreamWithFiles = for {
    dirs <- listOf1(genHdfsDirWithFiles)
  } yield dirs.toStream

}
