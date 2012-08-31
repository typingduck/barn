package barn

object HDFS {

  import org.apache.hadoop.util.GenericOptionsParser
  import org.apache.hadoop.fs.{Path, FileSystem}
  import org.apache.hadoop.conf.Configuration

  import scalaz._
  import Scalaz._

  type RemainingArgs = Array[String]

  def parseHadoopConf(args: Array[String]) : (RemainingArgs, Configuration) = {
    //Parse hadoop params and leave off extra ones
    val hadoopOptionParser = new GenericOptionsParser(new Configuration, args)
    val hadoopConf = hadoopOptionParser.getConfiguration()
    val remainingArgs = hadoopOptionParser.getRemainingArgs()
    (remainingArgs, hadoopConf)
  }

  def createPath(fs: FileSystem, path: Path) : Boolean = fs.mkdirs(path)

  def createFileSystem(conf: Configuration) : Validation[String, FileSystem]
  = FileSystem.get(conf).success
}

