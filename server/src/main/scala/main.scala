package barn

object BarnHdfsWriter extends App {

  import Stream.continually
  import scalaz._
  import Scalaz._
  import Logging._
  import BarnSteps._
  import Types._

  val (conf, rootLogDir, baseHdfsDir) = loadConf(args)

  val PS = HdfsPlacementStrategy

  val localTempDir = new Dir("/tmp")
  val excludeLocal = List("\\.gitignore", "target", "lock", "current", "\\..*")
  val excludeHdfs = List("tmp")

  val (shipInterval, retention) = (10, 60) //in seconds
  val minMB = 10 //minimum megabytes to keep for each service!

  continually(() => listSubdirectories(rootLogDir)).iterator.foreach {
    _().toOption.fold( _.foreach { serviceDir =>
      info("Checking service " + serviceDir + " to sync.")

      val result = for {
        fs           <- HDFS.createFileSystem(conf)

        serviceInfo  <- PS.decodeServiceInfo(serviceDir)

        hdfsDirs     <- PS.targetDirs(baseHdfsDir, serviceInfo)
        hdfsDir       = hdfsDirs.head
        hdfsTempDir  <- PS.targetTempDir(baseHdfsDir, serviceInfo)
        _            <- ensureHdfsDir(fs, hdfsDir)
        _            <- ensureHdfsDir(fs, hdfsTempDir)

        allHdfsFiles <- listHdfsFiles(fs, hdfsDir, excludeHdfs)
        hdfsFiles     = PS.sorted(PS.filter(allHdfsFiles, serviceInfo), serviceInfo)
        _            <- isShippingTime(hdfsFiles, shipInterval, PS.extractTS)

        localFiles   <- listLocalFiles(serviceDir, excludeLocal)
        candidates   <- outstandingFiles(localFiles, hdfsFiles, PS.extractTai)
        concatted    <- concatCandidates(candidates, localTempDir)

        hdfsName     <- PS.targetName(candidates, serviceInfo)
        hdfsTempFile <- shipToHdfs(fs, concatted, hdfsTempDir)
        _            <- atomicRenameOnHdfs(fs, hdfsTempFile, hdfsDir, hdfsName)

        cleanupLimit <- svlogdFileTimestamp(candidates.head)
        ♬            <- cleanupLocal(serviceDir, retention, cleanupLimit,
                                     minMB, excludeLocal)
      } yield ♬     // ♬ = tada! -- very limited in scope, hence the name.

      result ||| error

    }, error _)
  }
}


