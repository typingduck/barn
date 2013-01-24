package barn

import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime

trait Logging {

  val formatter = DateTimeFormat.forPattern("YYYY-MM-dd_HH:mm:ss.SSSSS")

  def now : String = formatter.print(DateTime.now)

  def info(s: String) = println("INFO " + s)
  def warn(s: String) = println("WARN " + s)
  def error(s: String) = println("ERROR " + s)

  def logBarnError(context: String)(s: BarnError)
  = error("[" + context + "] " + s.toString)

}

