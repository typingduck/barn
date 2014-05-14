package barn

object Hadoop extends Hadoop

trait Hadoop extends Logging {

  import org.apache.hadoop.util.GenericOptionsParser
  import org.apache.hadoop.fs.{Path, FileSystem => HdfsFileSystem}
  import org.apache.hadoop.conf.Configuration
  import scalaz._
  import Scalaz._
  import scala.util.control.Exception.catching
  import java.io.FileNotFoundException

  type LazyHdfsFileSystem = LazyWrapper[HdfsFileSystem]

  type RemainingArgs = Array[String]

  def parseHadoopConf(args: Array[String]) : (RemainingArgs, Configuration) = {
    //Parse hadoop params and leave off extra ones
    val hadoopOptionParser = new GenericOptionsParser(new Configuration, args)
    val hadoopConf = hadoopOptionParser.getConfiguration()
    val remainingArgs = hadoopOptionParser.getRemainingArgs()
    (remainingArgs, hadoopConf)
  }

  def ensureHdfsDir(fs: HdfsFileSystem, hdfsDir: HdfsDir)
  : BarnError \/ HdfsDir = validate(
    tap(hdfsDir)(createPath(fs, _)).right
  , "Can't ensure/create dirs on hdfs:"  + hdfsDir)

  def pathExists(fs: HdfsFileSystem, hdfsDir: HdfsDir)
  : BarnError \/ Boolean = validate(
    fs.exists(hdfsDir).right
  , "Can't check existence of the path:" + hdfsDir)

  def createPath(fs: HdfsFileSystem, path: Path) : Boolean = fs.mkdirs(path)

  def createFileSystem(conf: Configuration)
  : BarnError \/ HdfsFileSystem
  = HdfsFileSystem.get(conf).right

  def createLazyFileSystem(conf: Configuration)
  : BarnError \/ LazyHdfsFileSystem
  = new LazyWrapper(HdfsFileSystem.get(conf)).right

  def listHdfsFiles(fs: HdfsFileSystem, hdfsDir: HdfsDir)
  : BarnError \/ List[HdfsFile]
  = validate(
      catching(classOf[FileNotFoundException])
      .either(fs.listStatus(hdfsDir)).fold(
        _ => FileNotFound("Path " + hdfsDir + " doesn't exist to list") left,
        _ match {
          case null => FileNotFound("Path " + hdfsDir + " doesn't exist to list") left
          case fileList => fileList.toList.filterNot(_.isDir).map(_.getPath).right
        })
   , "Can't get list of files on HDFS dir: " + hdfsDir)

  def randomName() : String = scala.util.Random.alphanumeric.take(40).mkString

  def atomicShipToHdfs(fs: HdfsFileSystem
                     , src: File
                     , dest: HdfsDir
                     , hdfsName: String
                     , temp: HdfsDir)
  : BarnError \/ HdfsFile
  = for {
      hdfsTempFile <- shipToHdfs(fs, src, new HdfsDir(temp, randomName))
      renamedFile  <- atomicRenameOnHdfs(fs, hdfsTempFile, dest , hdfsName)
  } yield renamedFile

  def atomicRenameOnHdfs(fs: HdfsFileSystem
                       , src: HdfsFile
                       , dest: HdfsDir
                       , newName: String)
  : BarnError \/ HdfsFile
  = validate({
    val targetHdfsFile = new HdfsFile(dest, newName)
    info("Moving " + src + " to " + targetHdfsFile + " @ "  + fs.getUri)
    fs.rename(src, targetHdfsFile) match {
      case true => targetHdfsFile.right
      case false =>
        RenameFailed("Rename " + src + " to " + targetHdfsFile + " failed.").left
    }}, "Rename failed due to IO error")

  def shipToHdfs(fs: HdfsFileSystem, localFile: File, targetFile: HdfsFile)
  : BarnError \/ HdfsFile = validate ({
    info("Shipping " + localFile + " to " + targetFile + " @ " + fs.getUri)
    fs.copyFromLocalFile(true, true, new HdfsFile(localFile.getPath), targetFile)
    targetFile.right
  }, "Can't ship to hdfs from " + localFile + " to " + targetFile )

}

