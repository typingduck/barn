package barn

import Stream.continually
import scalaz._
import Scalaz._
import placement._

object BarnHdfsWriter
  extends App
  with Logging
  with Hadoop
  with SvlogdFile
  with FileCombiner
  with HdfsPlacementStrategy
  with LocalPlacementStrategy {

  val (conf, rootLogDir, baseHdfsDir) = loadConf(args)

  val localTempDir = new Dir("/tmp")
  val excludeLocal = List("\\..*")

  val (shipInterval, retention) = (10, 60) //in seconds
  val minMB = 10 //minimum megabytes to keep for each service!

  continually(() => listSubdirectories(rootLogDir)).iterator foreach {
    _().toOption.fold( _ foreach { serviceDir =>

      info("Checking service " + serviceDir + " to sync.")

      Thread.sleep(1000) //Replace me with iNotify and concurrent shippings

      val result = for {
        serviceInfo <- decodeServiceInfo(serviceDir)

        fs          <- createFileSystem(conf)
        plan        <- planNextShip(fs
                                  , serviceInfo
                                  , baseHdfsDir
                                  , shipInterval)

        localFiles  <- listSortedLocalFiles(serviceDir, excludeLocal)
        candidates  <- outstandingFiles(localFiles, plan lastTaistamp)
        concatted   <- concatCandidates(candidates, localTempDir)

        lastTaistamp = svlogdFileNameToTaiString(candidates.last.getName)
        targetName_  = targetName(lastTaistamp, serviceInfo)
        _           <- atomicShipToHdfs(fs
                                      , concatted
                                      , plan hdfsDir
                                      , targetName_
                                      , plan hdfsTempDir)

        shippedTS   <- svlogdFileTimestamp(candidates head)

        _           <- cleanupLocal(serviceDir
                                       , retention
                                       , shippedTS
                                       , minMB
                                       , excludeLocal)

      } yield ()

      result ||| logBarnError

    }, logBarnError _)
  }

  import org.apache.hadoop.conf.Configuration

  def loadConf(args: Array[String]) : (Configuration, Dir, HdfsDir) = {
    val (remainingArgs, conf) = parseHadoopConf(args)
    val rootLogDir = new Dir(remainingArgs(0))
    val rootHdfsDir = new HdfsDir(remainingArgs(1))
    (conf, rootLogDir, rootHdfsDir)
  }

  def outstandingFiles(localFiles: List[File], lastTaistamp: Option[String])
  : Validation[BarnError, List[File]]
  = lastTaistamp match {
    case Some(taistamp) =>
      localFiles dropWhile(f =>
        svlogdFileNameToTaiString(f getName) <= taistamp) match {
        case Nil => NothingToSync("No local files left to sync.") fail
        case x => x success
      }
    case None => localFiles success  //All files are left to be synced
  }

}


