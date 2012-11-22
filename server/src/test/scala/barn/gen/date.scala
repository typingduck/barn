package barn

object DateGenerators extends DateGenerators

trait DateGenerators {

  import org.scalacheck.Gen._
  import org.scalacheck.Gen

  import org.joda.time._
  import Tai64._

  val baseDate = new DateTime(1970, 1, 1, 0, 0, 0, 0, DateTimeZone.UTC);

  def genDate(after: DateTime = baseDate) = for {
    second <- choose(1, 60 * 365 * 24 * 60 * 60)
  } yield baseDate.plusSeconds(second)

  def genTai64Date(after: DateTime = baseDate) = for {
    date <- genDate(after)
  } yield convertDateToTai64(date)

}
