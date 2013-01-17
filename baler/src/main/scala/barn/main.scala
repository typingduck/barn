package barn

import Stream.continually
import scalaz._
import Scalaz._
import placement._

import org.apache.hadoop.conf.{Configuration => HadoopConf}
import org.joda.time._

object BarnHdfsWriter
  extends App
  with Logging
  with Hadoop
  with SvlogdFile
  with FileCombiner
  with HdfsPlacementStrategy
  with LocalPlacementStrategy
  with ParamParser
  with TimeUtils {

  val (shipInterval, retention) = (10, 60) //in seconds
  val minMB = 10 //minimum megabytes to keep for each service!
  val defaultLookBackDays = 10

  loadConf(args) { barnConf =>
    continually(() => listSubdirectories(barnConf.localLogDir)).iterator foreach {
      _().fold(logBarnError
             , syncRootLogDir(barnConf))
    }
  }

  def syncRootLogDir(barnConf: BarnConf)(dirs: List[Dir])
  : Unit = dirs match {
    case Nil =>
      info("No service has appeared in root log dir. Incorporating patience.")
      Thread.sleep(1000)
    case xs =>
      (if(barnConf.runParallel) xs.par else xs) map { serviceDir =>

        info("Checking service " + serviceDir + " to sync.")
        Thread.sleep(1000) //Replace me with iNotify

        val result = for {
          serviceInfo <- decodeServiceInfo(serviceDir)
          fs          <- createFileSystem(barnConf.hdfsEndpoint)
          localFiles  <- listSortedLocalFiles(serviceDir)
          lookBack    <- earliestLookbackDate(localFiles, defaultLookBackDays)
          plan        <- planNextShip(fs
                                    , serviceInfo
                                    , barnConf.hdfsLogDir
                                    , shipInterval
                                    , lookBack)
          candidates  <- outstandingFiles(localFiles, plan lastTaistamp)
          concatted   <- concatCandidates(candidates, barnConf.localTempDir)
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
                                         , minMB)
        } yield ()

        result ||| logBarnError
      }
  }

  def earliestLookbackDate(localFiles: List[File], defaultLookBackDays: Int)
  : Validation[BarnError, DateTime]
   = inceptRight(localFiles.lastOption.map(svlogdFileTimestamp))
      .map(_ getOrElse DateTime.now.minusDays(defaultLookBackDays))

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


