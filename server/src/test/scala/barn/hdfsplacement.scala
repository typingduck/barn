package barn

import scalaz._
import Scalaz._

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._
import org.apache.hadoop.conf.Configuration
import barn.placement.HdfsPlacementStrategy
import org.joda.time._

class HdfsPlacementSuite
  extends FunSuite
  with Checkers
  with HdfsPlacementStrategy
  with PlacementGenerators
  with DateGenerators
  with HadoopGenerators
  with TimeUtils {

  import barn.placement.ShippingPlan

  val conf = tap(new Configuration()){_.set("fs.default.name"
                                          , "hdfs://localhost:9000")}
  val fs = Hadoop.createFileSystem(conf).toOption.get

  test("Should plan to ship all files if no file is yet shipped") {

      val shippingInterval = 0

      check(

        forAll(genLocalServiceInfo
             , genHdfsDirectory()) {

          case (serviceInfo, baseDir) =>

            val lookBack = DateTime.now.minusDays(10)

            planNextShip(fs
                       , serviceInfo
                       , baseDir
                       , shippingInterval
                       , lookBack) match {

              case Failure(e) => false :| "Failed to plan to ship with " + e
              case Success(shippingPlan) =>

                val now = DateTime.now
                val correctPlan =
                  ShippingPlan(targetDirs(now, baseDir, lookBack).head
                             , targetTempDir(baseDir)
                             , None)

                (shippingPlan == correctPlan) :| "Wrong plan deduced. Got:" +
                                                shippingPlan +
                                               " Wanted:" + correctPlan

              case other => false :| "Plan isn't expected" + other
            }
        })
   }

  test("Should plan to ship all files that hasn't yet been shipped.") {

    val shippingInterval = 0

    check(

      forAll(genLocalServiceInfo
           , genHdfsDirectory()
           , genDateBeforeNow(40)) {

        case (serviceInfo, baseDir, lastShippedDate) =>

          val now = DateTime.now
          val targetDir = targetDirs(now, baseDir, lastShippedDate).head
          val lastTaistamp = Tai64.convertDateToTai64(lastShippedDate)

          val lastShippedFile =
            new HdfsFile(targetDir, targetName(lastTaistamp, serviceInfo))

          fs.create(lastShippedFile)

          val correctPlan = ShippingPlan(targetDir
                                       , targetTempDir(baseDir)
                                       , Some(lastTaistamp))


          planNextShip(fs
                     , serviceInfo
                     , baseDir
                     , shippingInterval
                     , lastShippedDate.minusDays(1)) match {

            case Failure(e) => false :| "Failed to plan to ship with " + e
            case Success(shippingPlan) =>

              (shippingPlan == correctPlan) :| "Wrong plan deduced. Got:" +
                                              shippingPlan +
                                             " Wanted:" + correctPlan

            case other => false :| "Plan isn't expected" + other
          }
      })
   }


  test("Should refrain from shipping if shippingInterval isnt't reached") {

    check(

      forAll(genLocalServiceInfo
           , genHdfsDirectory()
           , genDateBeforeNow(40)
           , genShippingInterval) {

        case (serviceInfo
            , baseDir
            , lastShippedDate
            , shippingInterval) =>

          val now = DateTime.now
          val targetDir = targetDirs(now, baseDir, lastShippedDate).head
          val lastTaistamp = Tai64.convertDateToTai64(lastShippedDate)
          val targetName_ = targetName(lastTaistamp, serviceInfo)
          val lastShippedFile = new HdfsFile(targetDir, targetName_)
          val shouldThrottle = enoughTimePast(lastShippedDate, shippingInterval)

          fs.create(lastShippedFile).close

          planNextShip(fs
                     , serviceInfo
                     , baseDir
                     , shippingInterval
                     , lastShippedDate.minusDays(1)) match {
            case Failure(SyncThrottled(_)) =>
              shouldThrottle :| "Sync throttled though it shouldn't have been"
            case Failure(e) => false :| "Failed to plan to ship with " + e
            case Success(shippingPlan) =>

              val correctPlan = ShippingPlan(targetDir
                                           , targetTempDir(baseDir)
                                           , Some(lastTaistamp))

              (shippingPlan == correctPlan) :| "Wrong plan deduced. Got:" +
                                              shippingPlan +
                                             " Wanted:" + correctPlan

            case other => false :| "Plan isn't expected" + other
          }

          fs.delete(lastShippedFile, true)
      })
  }

}
