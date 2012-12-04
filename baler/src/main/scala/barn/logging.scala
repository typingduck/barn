package barn

import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime

trait Logging {

  val formatter = DateTimeFormat.forPattern("YYYY-MM-dd_HH:mm:ss.SSSSS")

  def now : String = formatter.print(DateTime.now)

  def info(s: String) = println(now + "  INFO " + s)
  def warn(s: String) = println(now + "  WARN " + s)
  def error(s: String) = println(now + " ERROR " + s)

  def logBarnError(s: BarnError) = error(s.toString)

}

