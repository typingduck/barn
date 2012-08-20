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
import org.joda.time._

object BarnHdfsWriter extends App {

  import LogFiles._

  val (remainingArgs, conf) = Hdfsfsfsfs.parseHadoopConf(args)
  val fs = Hdfsfsfsfs.createFileSystem(conf)

  val rootLogPath = remainingArgs(0)
  val exclude = List(".gitignore", "target", "lock", "current")  //Dev, remove me
  val baseHdfsPath = "/user/omid/barn-test/"

  def timeToFlush(lastWrittenDate: DateTime) : Boolean = {
    System.out.println("lastWrittenDate: " + lastWrittenDate)
    System.out.println("            now: " + DateTime.now)
    !new Interval(lastWrittenDate, DateTime.now(DateTimeZone.UTC)).toDuration
                                              .toStandardSeconds
                                              .isLessThan(Seconds.seconds(60))
  }


  while(true) {

    //Inotify doesn't work on OSX so for now I'm sleeping!
    Thread.sleep(1000)

    val servicesDirs : List[File] = new File(rootLogPath)
                                  .listFiles
                                  .toList
                                  .filter(x => x.isDirectory)
                                  .sorted

    servicesDirs.foreach { service : File =>
      System.out.println("service : " + service)

      service.listFiles
             .toList
             .filterNot(x => exclude.contains(x.getName))
             .sorted match {

        case null =>
          System.out.println("ERROR: For some reason I can't list files in " + service)

        case localFiles =>

          //Service log path on HDFS
          val remotePath = new Path(baseHdfsPath + service.getName)

          //Make sure the path is there
          Hdfsfsfsfs.createPath(fs, remotePath)

          //Grab list of files on HDFS
          val remoteFiles = fs.listStatus(remotePath).toList.map(_.getPath.getName).sorted

          (remoteFiles.lastOption match {
            case Some(last) =>
              val lastTimestamp = Tai64.convertTai64ToTime(last.drop(1))
              System.out.println("Last written date: " + last)
              localFiles.dropWhile(_.getName <= last) match {
                case Nil => None
                case x if timeToFlush(lastTimestamp) => Some(x)
                case _ =>
                  System.out.println("Nope, it's a bit of time ago I flushed. Next time!")
                  None
              }
            case _ => Some(localFiles)
          }).map { candidates =>
              val combinedName = "/tmp/" + candidates.last.getName

              System.out.println("I'm gonna combine " + candidates)

              FileCombiner.combineIntoSeqFile(candidates , new File(combinedName))
              fs.copyFromLocalFile(true, true, new Path(combinedName), remotePath)

              System.out.println("Done combining! Auf wiedersehen!")
          }
      }
    }
  }
}
