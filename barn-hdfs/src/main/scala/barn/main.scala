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

  val minMB = 1 //minimum megabytes to keep for each service!
  val maxLookBackDays = 10
  val excludeList = List("""^\..*""") //Exclude files starting with dot (temp)

  loadConf(args) { barnConf => {

    enableGanglia(barnConf.appName
               , GangliaOpts(barnConf.gangliaHost
                           , barnConf.gangliaPort))

    continually(() => listSubdirectories(barnConf.localLogDir)).iterator
      .foreach { listDirs => {

        info("Round of sync started.")

        listDirs().fold(logBarnError("List dirs in" + barnConf.localLogDir)
                      , syncRootLogDir(barnConf))

        info("Round of sync finished. Sleeping.")
        Thread.sleep(600000) //Replace me with iNotify //TODO make me config'able
      }
    }
  }}

  def syncRootLogDir(barnConf: BarnConf)(dirs: List[Dir])
  : Unit = dirs match {
    case Nil =>
      info("No service has appeared in root log dir. Incorporating patience.")
      Thread.sleep(1000)
    case xs =>

     import scala.collection.JavaConverters._
     val hdfsListCache : HdfsListCache = new HdfsListCacheJ asScala

     //Working around SI-4843 https://issues.scala-lang.org/browse/SI-4843
     if(barnConf.runParallel) {
        import scala.collection.parallel._
        import scala.concurrent._

        val xsPar = xs.par
        xsPar.tasksupport = new ForkJoinTaskSupport(
                              new forkjoin.ForkJoinPool(barnConf.degParallel))

        xsPar map actOnServiceDir(barnConf, hdfsListCache)
      } else
        xs map actOnServiceDir(barnConf, hdfsListCache)
  }

  def actOnServiceDir(barnConf: BarnConf, hdfsListCache: HdfsListCache)
                     (serviceDir : Dir) = {

    reportOngoingSync {

    val result = for {
      serviceInfo <- decodeServiceInfo(serviceDir)
      fs          <- createLazyFileSystem(barnConf.hdfsEndpoint)
      localFiles  <- listSortedLocalFiles(serviceDir, excludeList)
      lookBack    <- earliestLookbackDate(localFiles, maxLookBackDays)
      plan        <- planNextShip(fs
                                , serviceInfo
                                , barnConf.hdfsLogDir
                                , barnConf.shipInterval
                                , lookBack
                                , hdfsListCache)

      candidates  <- outstandingFiles(localFiles, plan lastTaistamp, maxLookBackDays)
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

    result.leftMap(reportError("Sync of " + serviceDir + "") _)

    }
  }

  def reportError(context: String)(e: BarnError) = e match {
    case _ : BarnFatalError => {
      logBarnError(context)(e)
      reportFatalError
    }
    case _ => ()
  }

  def earliestLookbackDate(localFiles: List[File], maxLookBackDays: Int)
  : Validation[BarnError, DateTime] = {
    val maxLookBackTime = DateTime.now.minusDays(maxLookBackDays)

    localFiles.headOption match {
      case Some(f) =>
        svlogdFileTimestamp(f).map(ts =>
          if(ts.isBefore(maxLookBackTime)) maxLookBackTime else ts)
      case None => maxLookBackTime.success
    }
  }

  def outstandingFiles(localFiles: List[File], lastTaistamp: Option[String], maxLookBackTime: Int)
  : Validation[BarnError, List[File]] = {
    val maxLookBackTime = DateTime.now.minusDays(maxLookBackDays).minusDays(1).toDateMidnight

    lastTaistamp match {
      case Some(taistamp) =>
        localFiles dropWhile(f => {

            val fileTaistring = svlogdFileNameToTaiString(f getName)

            fileTaistring <= taistamp ||  //TODO Deduplicate me with the case below
              Tai64.convertTai64ToTime(fileTaistring).isBefore(maxLookBackTime)

          }) match {
            case Nil => NothingToSync("No local files left to sync.") fail
            case x => x success
          }
      case None =>
        localFiles.dropWhile(f =>    //TODO Deduplicate me with the case above
          Tai64.convertTai64ToTime(
            svlogdFileNameToTaiString(f getName))
              .isBefore(maxLookBackTime)) match {
                case Nil => NothingToSync("No local files left to sync.") fail
                case x => x success
               }
    }
  }
}


