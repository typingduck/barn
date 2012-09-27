package barn

import org.joda.time._

object TimeUtils {

  def enoughTimePast(base: DateTime, enoughSeconds: Int)
  : Boolean = !(new Interval(base, DateTime.now(DateTimeZone.UTC))
    .toDuration.toStandardSeconds isLessThan Seconds.seconds(enoughSeconds))

}
