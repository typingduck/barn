package barn

object Types {

  import java.io.{File => File_}
  import org.apache.hadoop.fs.Path

  /*
    File/Dir/Path on Local/Hdfs legend for the confusionary situation:

    type Dir -> local directory
    type File -> local File
    type HdfsDir -> hdfs directory
    type HdfsFile -> hdfs file

    the word "path is intentionally avoided as much as possible.
  */
  type Dir = File_
  type File = File_
  type HdfsDir = HdfsFile
  type HdfsFile = Path

}
