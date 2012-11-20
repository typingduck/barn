package barn

import org.apache.hadoop.fs.{FileSystem}
import org.apache.hadoop.io.SequenceFile
import org.apache.commons.io.IOUtils
import java.io.{FileInputStream, FileOutputStream}
import scala.io.Source
import org.apache.hadoop.io.{BytesWritable => BW, LongWritable => LW, SequenceFile}
import org.apache.hadoop.conf.Configuration
import sun.misc.BASE64Decoder;
import scalaz._
import Scalaz._
import org.apache.commons.lang.RandomStringUtils

object FileCombiner extends FileCombiner

trait FileCombiner extends Logging {

  //Combines files into a single file
  def combineFiles(localFiles: List[File], outputFile: File) = {
    val inputStreams = localFiles.map(x => new FileInputStream(x))
    val outputStream = new FileOutputStream(outputFile)

    inputStreams.foreach(is => IOUtils.copy(is, outputStream))

    inputStreams.foreach (_.close)
    outputStream.close
  }

  //Combines local files into a local combined sequence file
  def combineIntoSeqFile(localFiles: List[File]
                       , outputFile: File
                       , f : String => Array[Byte]) = {

    val conf = new Configuration
    val fs = FileSystem.getLocal(conf)

    val outputWriter =
      new SequenceFile.Writer( fs
                             , conf
                             , new HdfsDir(outputFile.getAbsolutePath)
                             , classOf[LW]
                             , classOf[BW]
                             )

    localFiles.foreach( file =>
    Source.fromFile(file).getLines.foreach( line =>
      outputWriter.append( new LW(System.currentTimeMillis())
                         , new BW(f(line)))
    ))

    outputWriter.close
    fs.close

  }

  val BASE64 = "base64"
  val syslogMatcher = "(?:^.*?\\[origin.*?x-encoding\\s*?=\\s*?\"\\s*(.*?)\\s*\".*?\\]\\s)?(.*)".r
  val base64Decoder = new BASE64Decoder();

  def processFormat(line: String) : Array[Byte]
  = syslogMatcher.findFirstMatchIn(line) match {
      case Some(syslogMatcher(BASE64,body)) => base64Decoder.decodeBuffer(body)
      case Some(_) => line.getBytes
      case None => throw new java.lang.Exception("Regexp match to extract body failed?? Come save me!") //FIXME
    }

  def concatCandidates(candidates: List[File], targetDir: Dir)
  : Validation[String, File] = validate ({
    val combinedName = RandomStringUtils.randomAlphanumeric(20)
    val combinedLocalFile = new File(targetDir, combinedName)
    info("Combining " + candidates.size + " into " + combinedLocalFile)
    combineIntoSeqFile(candidates, combinedLocalFile, processFormat)
    combinedLocalFile success
  } , "Can't combine files into a sequence file." +
      " Candidates to combine: " + candidates)

}


