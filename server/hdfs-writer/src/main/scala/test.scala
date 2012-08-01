import java.io.File
import org.apache.hadoop.util.GenericOptionsParser
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.io.{BytesWritable => BW, LongWritable => LW, SequenceFile}
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{Path, FileSystem}
import org.apache.hadoop.io.SequenceFile
import java.io.FileInputStream
import java.io.FileOutputStream
import org.apache.commons.io.IOUtils
import java.text.SimpleDateFormat
import java.util.Date

object LogFiles {
  import org.joda.time.DateTime

  def getLatest(times: List[DateTime]) : DateTime = {
    implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)
    times.sorted.head
  }

  def tai2Human(name: String) : DateTime
  = Tai64.convertTai64ToTime(name)

  def svLogdName2tai(name: String) : String
  = name.substring(1, name.size - 2)
}

object FileCombiner {

  def combineFiles(localFiles: List[File], outputFile: File) = {
    val inputStreams = localFiles.map(x => new FileInputStream(x))
    val outputStream = new FileOutputStream(outputFile)

    inputStreams.foreach(is => IOUtils.copy(is, outputStream))

    inputStreams.foreach (_.close)
    outputStream.close
  }

}

object BarnHdfsWriter extends App {

  import LogFiles._

  //Parse hadoop params and leave off extra ones
  val hadoopOptionParser = new GenericOptionsParser(new Configuration, args)
  val hadoopConf = hadoopOptionParser.getConfiguration()
  val remainingArgs = hadoopOptionParser.getRemainingArgs()
  val fs = FileSystem.get(hadoopConf)

  val logPath = remainingArgs(0)
  val exclude = List(".gitignore", "target", "lock", "current")  //Dev, remove me

  while(true) {
    val localFiles : List[File] = new File(logPath)
                                .listFiles
                                .toList
                                .filterNot(x => exclude.contains(x.getName))
                                .sorted

    val localFileNames = localFiles.map(_.getName)
    val remotePath = new Path("/user/omid/barn-test")
    val remoteFiles = fs.listStatus(remotePath).toList.map(_.getPath.getName).sorted

    //Find the first occurence of the last localFile

    System.out.println("Checking out " + new SimpleDateFormat("yyyy/MM/dd HH:mm:ss").format(new Date()))
    Thread.sleep(1000)
    (remoteFiles.lastOption match {
      case Some(last) => localFiles.dropWhile(_.getName <= last) match {
        case Nil => None
        case x => Some(x)
      }
      case _ => localFiles.lastOption.map(_ => localFiles)
    }).map { candidates =>
        val combinedName = "/tmp/" + candidates.last.getName

        System.out.println("I'm gonna combine " + candidates)

        FileCombiner.combineFiles(candidates , new File(combinedName))
        fs.copyFromLocalFile(true, true, new Path(combinedName), remotePath)

        System.out.println("Done combining! Auf wiedersehen!")
    }

  }
}
