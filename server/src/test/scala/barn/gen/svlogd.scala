package barn

object SvlogdGenerators extends SvlogdGenerators

trait SvlogdGenerators extends DateGenerators {

  import org.apache.hadoop.fs.{FileSystem => HdfsFileSystem}

  import org.scalacheck.Gen._
  import org.scalacheck.Gen

  val genSvlogdFileName = for {
    date <- genTai64Date(baseDate)
  } yield "@" + date + ".s"
}
