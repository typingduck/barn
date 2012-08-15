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

object BarnHdfsWriter extends App {

  import LogFiles._

  val (remainingArgs, conf) = Hdfsfsfsfs.parseHadoopConf(args)
  val fs = Hdfsfsfsfs.createFileSystem(conf)

  val rootLogPath = remainingArgs(0)
  val exclude = List(".gitignore", "target", "lock", "current")  //Dev, remove me
  val baseHdfsPath = "/user/omid/barn-test/"

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

      service.listFiles.toList.filterNot(x => exclude.contains(x.getName)) match {
        case null =>
          System.out.println("ERROR: For some reason I can't list files in " + service)
        case localFiles =>

          val remotePath = new Path(baseHdfsPath + service.getName)

          //Make sure the path is there
          Hdfsfsfsfs.createPath(fs, remotePath)
          val remoteFiles = fs.listStatus(remotePath).toList.map(_.getPath.getName).sorted

          //System.out.println("Checking out " + new SimpleDateFormat("yyyy/MM/dd HH:mm:ss").format(new Date()))

          (remoteFiles.lastOption match {
            case Some(last) => localFiles.dropWhile(_.getName <= last) match {
              case Nil => None
              case x => Some(x)
            }
            case _ => localFiles.lastOption.map(_ => localFiles)
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
