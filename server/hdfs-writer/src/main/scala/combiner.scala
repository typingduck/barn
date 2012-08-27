package barn

import java.io.File
import org.apache.hadoop.fs.{Path, FileSystem}
import org.apache.hadoop.io.SequenceFile
import org.apache.commons.io.IOUtils
import java.io.{FileInputStream, FileOutputStream}
import scala.io.Source
import org.apache.hadoop.io.{BytesWritable => BW, LongWritable => LW, SequenceFile}
import org.apache.hadoop.conf.Configuration

object FileCombiner {

  //Combines files into a single file
  def combineFiles(localFiles: List[File], outputFile: File) = {
    val inputStreams = localFiles.map(x => new FileInputStream(x))
    val outputStream = new FileOutputStream(outputFile)

    inputStreams.foreach(is => IOUtils.copy(is, outputStream))

    inputStreams.foreach (_.close)
    outputStream.close
  }

  //Combines local files into a local combined sequence file
  def combineIntoSeqFile(localFiles: List[File], outputFile: File) = {

    val conf = new Configuration
    val fs = FileSystem.getLocal(conf)

    val outputWriter =
      new SequenceFile.Writer( fs
                             , conf
                             , new Path(outputFile.getAbsolutePath)
                             , classOf[LW]
                             , classOf[BW]
                             )

    localFiles.foreach( file =>
    Source.fromFile(file).getLines.foreach( line =>
      outputWriter.append( new LW(System.currentTimeMillis())
                         , new BW(line.getBytes))
    ))

    outputWriter.close
    fs.close

  }

}


