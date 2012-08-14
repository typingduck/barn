package barn

object LogFiles {
  import org.joda.time.DateTime

  def getLatest(times: List[DateTime]) : DateTime = {
    implicit def dateTimeOrdering: Ordering[DateTime] = Ordering.fromLessThan(_ isBefore _)
    times.sorted.head
  }

  def tai2Human(name: String) : DateTime
  = Tai64.convertTai64ToTime(name)

  def svLogdName2tai(name: String) : String
  = name.substring(1, name.size - 2)
}

