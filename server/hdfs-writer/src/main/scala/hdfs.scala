package barn

import java.io.File
import org.apache.hadoop.util.GenericOptionsParser
import org.apache.hadoop.io.{BytesWritable => BW, LongWritable => LW, SequenceFile}
import org.apache.hadoop.fs.{Path, FileSystem}
import org.apache.hadoop.io.SequenceFile
import java.io.{FileInputStream, FileOutputStream, InputStreamReader, BufferedReader}
import org.apache.commons.io.IOUtils
import java.text.SimpleDateFormat
import java.util.Date
import scala.io.Source
import org.apache.hadoop.io.{BytesWritable => BW, LongWritable => LW, SequenceFile}
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{Path, LocalFileSystem, RawLocalFileSystem}
import org.apache.hadoop.io.SequenceFile
import scalaz._
import Scalaz._

object HDFS {

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

