package barn

import com.yammer.metrics.scala.Instrumented;
import java.util.concurrent.TimeUnit
import java.net.InetAddress

trait Instruments extends Instrumented {

  private val currentlyShipping = metrics.counter("currentlyShipping")
  private val fatalError = metrics.counter("fatalError")
  private val combineTime = metrics.timer("combineTime")
  private val shipTime = metrics.timer("shipTime")
  private val cleanupTime = metrics.timer("cleanupTime")

  def reportShippingStarted = currentlyShipping += 1
  def reportShippingEnded = currentlyShipping -= 1

  def reportOngoingSync[A](f: => A) {
    currentlyShipping += 1
    try f finally currentlyShipping -= 1
  }

  def reportFatalError = fatalError += 1
  def reportCombineTime[A](f: => A) = combineTime.time(f)
  def reportShipTime[A](f: => A) = shipTime.time(f)
  def reportCleanupTime[A](f: => A) = cleanupTime.time(f)

  import com.yammer.metrics.ganglia.GangliaReporter

  case class GangliaOpts
    ( host:  String
    , port:     Int
    , interval: Int = 5
    )

  def enableGanglia(app: String, opts: GangliaOpts): Unit =
    new GangliaReporter(opts.host, opts.port) {
      lazy val spoof = List.fill(2) {
        "%s-%s" format (app, InetAddress.getLocalHost.getHostName)
      }.mkString(":")

      override def getDMAX      = 3600  // discard after an hour
      override def getHostLabel = spoof
    }.start(opts.interval, TimeUnit.SECONDS)

}
