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
  with TimeUtils
  with Instruments {

  val minMB = 10 //minimum megabytes to keep for each service!
  val defaultLookBackDays = 10
  val excludeList = List("""^\..*""") //Exclude files starting with dot (temp)

  loadConf(args) { barnConf => {

    enableGanglia(barnConf.appName
               , GangliaOpts(barnConf.gangliaHost
                           , barnConf.gangliaPort))

    continually(() => listSubdirectories(barnConf.localLogDir)).iterator
      .foreach { listDirs => {

        listDirs().fold(logBarnError("List dirs in" + barnConf.localLogDir)
                      , syncRootLogDir(barnConf))

        Thread.sleep(1000) //Replace me with iNotify
      }
    }
  }}

  def syncRootLogDir(barnConf: BarnConf)(dirs: List[Dir])
  : Unit = dirs match {
    case Nil =>
      info("No service has appeared in root log dir. Incorporating patience.")
      Thread.sleep(1000)
    case xs =>
     //Working around SI-4843 https://issues.scala-lang.org/browse/SI-4843
     if(barnConf.runParallel)
        xs.par map actOnServiceDir(barnConf)
      else
        xs map actOnServiceDir(barnConf)
  }

  def actOnServiceDir(barnConf: BarnConf)(serviceDir : Dir) = {

    reportOngoingSync {

    val result = for {
      serviceInfo <- decodeServiceInfo(serviceDir)
      fs          <- createFileSystem(barnConf.hdfsEndpoint)
      localFiles  <- listSortedLocalFiles(serviceDir, excludeList)
      lookBack    <- earliestLookbackDate(localFiles, defaultLookBackDays)
      plan        <- planNextShip(fs
                                , serviceInfo
                                , barnConf.hdfsLogDir
                                , barnConf.shipInterval
                                , lookBack)

      candidates  <- outstandingFiles(localFiles, plan lastTaistamp)
      concatted   <- reportCombineTime(
                       concatCandidates(candidates, barnConf.localTempDir))

      lastTaistamp = svlogdFileNameToTaiString(candidates.last.getName)
      targetName_  = targetName(lastTaistamp, serviceInfo)

      _           <- reportShipTime(
                      atomicShipToHdfs(fs
                                    , concatted
                                    , plan hdfsDir
                                    , targetName_
                                    , plan hdfsTempDir))

      shippedTS   <- svlogdFileTimestamp(candidates last)
      _           <- cleanupLocal(serviceDir
                                , shippedTS
                                , minMB)
    } yield ()

    result ||| reportError("Sync of " + serviceDir + "")

    }
  }

  def reportError(context: String)(e: BarnError) = e match {
    case _ : BarnFatalError => {
      logBarnError(context)(e)
      reportFatalError
    }
    case _ => ()
  }

  def earliestLookbackDate(localFiles: List[File], defaultLookBackDays: Int)
  : Validation[BarnError, DateTime]
   = inceptRight(localFiles.headOption.map(svlogdFileTimestamp))
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


