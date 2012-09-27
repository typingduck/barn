package barn

trait HDFS extends Logging {

  import org.apache.hadoop.util.GenericOptionsParser
  import org.apache.hadoop.fs.{Path, FileSystem => HdfsFileSystem}
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

  def ensureHdfsDir(fs: HdfsFileSystem, hdfsDir: HdfsDir)
  : Validation[String, HdfsDir] = validate(
    tap(hdfsDir)(createPath(fs, _)).success
  , "Can't ensure/create dirs on hdfs:"  + hdfsDir)

  def createPath(fs: HdfsFileSystem, path: Path) : Boolean = fs.mkdirs(path)

  def createFileSystem(conf: Configuration)
  : Validation[String, HdfsFileSystem]
  = HdfsFileSystem.get(conf).success

  def listHdfsFiles(fs: HdfsFileSystem, hdfsDir: HdfsDir)
  : Validation[String, List[HdfsFile]]
  = validate(
      fs.listStatus(hdfsDir)
        .toList
        .filterNot(x => x.isDir)
        .map(_.getPath)
        .success
   , "Can't get list of files on HDFS dir: " + hdfsDir)

  def randomName() : String = "hello"

  def atomicShipToHdfs(fs: HdfsFileSystem
                     , src: File
                     , dest: HdfsDir
                     , hdfsName: String
                     , temp: HdfsDir) : Validation[String, HdfsFile]
  = for {
      hdfsTempFile <- shipToHdfs(fs, src, new HdfsDir(temp, randomName))
      renamedFile  <- atomicRenameOnHdfs(fs, hdfsTempFile, dest , hdfsName)
  } yield renamedFile

  def atomicRenameOnHdfs(fs: HdfsFileSystem
                       , src: HdfsFile
                       , dest: HdfsDir
                       , newName: String) : Validation[String, HdfsFile]
  = validate({
    val targetHdfsFile = new HdfsFile(dest, newName)
    info("Moving " + src + " to " + targetHdfsFile + " @ "  + fs.getUri)
    fs.rename(src, targetHdfsFile) match {
      case true => targetHdfsFile.success
      case false => ("Rename " + src + " to " + dest + " failed.").fail
    }}, "Rename failed due to IO error")

  def shipToHdfs(fs: HdfsFileSystem,
                 localFile: File,
                 hdfsDir: HdfsDir)
  : Validation[String, HdfsFile] = validate ({
    info("Shipping " + localFile + " to " + hdfsDir + " @ " + fs.getUri)
    fs.copyFromLocalFile(true, true, new HdfsFile(localFile.getPath), hdfsDir)
    hdfsDir.suffix("/" + localFile.getName).success
  }, "Can't ship to hdfs from " + localFile + " to " + hdfsDir )

}

