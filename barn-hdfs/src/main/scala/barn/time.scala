package barn

import org.joda.time._

object TimeUtils extends TimeUtils

trait TimeUtils {

  def enoughTimePast(base: DateTime, enoughSeconds: Int)
  : Boolean = !(new Interval(base, DateTime.now(DateTimeZone.UTC))
    .toDuration.toStandardSeconds isLessThan Seconds.seconds(enoughSeconds))

  def daysAgo(days: Int) : DateTime = DateTime.now.minusDays(days)

}
