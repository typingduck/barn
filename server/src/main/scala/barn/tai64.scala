package barn

object Tai64 extends Tai64

trait Tai64 {

  import org.joda.time._
  import org.apache.commons.lang.StringUtils.leftPad

  val baseDate = new DateTime(1970, 1, 1, 0, 0, 0, 0, DateTimeZone.UTC);
  val taiBase = 4611686018427387904L

  def convertDateToTai64(time: DateTime) : String = {
    val extraSeconds = conversion_table.reverse.find(_._1.isBefore(time)).map(_._2).getOrElse(0)
    val leapAdjusted = time.plusSeconds(extraSeconds)
    val timedelta = new Duration(baseDate, leapAdjusted)
    val seconds = timedelta.getStandardSeconds
    val nanoInt = (timedelta.getMillis - timedelta.getStandardSeconds * 1000) * 1000000
    val taiInt = seconds + taiBase
    val intHex = java.lang.Long.toHexString(taiInt)
    val nanoHex = java.lang.Long.toHexString(nanoInt)
    leftPad(intHex, 16, '0') + leftPad(nanoHex, 8, '0')
  }

  def convertTai64ToTime(hex : String) : DateTime = {
    val taiInt = java.lang.Long.parseLong(hex.substring(0,16), 16)
    val nanoInt = java.lang.Long.parseLong(hex.substring(16, 24), 16)
    val seconds = taiInt - taiBase
    val timedelta = new Duration(seconds * 1000 + nanoInt / 1000000)
    val final_ = baseDate.plus(timedelta)
    val extraSeconds : Int = conversion_table.reverse.find(_._1.isBefore(final_)).map(_._2).getOrElse(0)
    final_.plusSeconds(extraSeconds * -1)
  }

  val conversion_table = List[(DateTime, Int)](
                      (new DateTime(1972, 01,  1, 0 , 0), 10),
                      (new DateTime(1972, 07,  1, 0 , 0), 11),
                      (new DateTime(1973, 01,  1, 0 , 0), 12),
                      (new DateTime(1974, 01,  1, 0 , 0), 13),
                      (new DateTime(1975, 01,  1, 0 , 0), 14),
                      (new DateTime(1976, 01,  1, 0 , 0), 15),
                      (new DateTime(1977, 01,  1, 0 , 0), 16),
                      (new DateTime(1978, 01,  1, 0 , 0), 17),
                      (new DateTime(1979, 01,  1, 0 , 0), 18),
                      (new DateTime(1980, 01,  1, 0 , 0), 19),
                      (new DateTime(1981, 07,  1, 0 , 0), 20),
                      (new DateTime(1982, 07,  1, 0 , 0), 21),
                      (new DateTime(1983, 07,  1, 0 , 0), 22),
                      (new DateTime(1985, 07,  1, 0 , 0), 23),
                      (new DateTime(1988, 01,  1, 0 , 0), 24),
                      (new DateTime(1990, 01,  1, 0 , 0), 25),
                      (new DateTime(1991, 01,  1, 0 , 0), 26),
                      (new DateTime(1992, 07,  1, 0 , 0), 27),
                      (new DateTime(1993, 07,  1, 0 , 0), 28),
                      (new DateTime(1994, 07,  1, 0 , 0), 29),
                      (new DateTime(1996, 01,  1, 0 , 0), 30),
                      (new DateTime(1997, 07,  1, 0 , 0), 31),
                      (new DateTime(1999, 01,  1, 0 , 0), 32),
                      (new DateTime(2006, 01,  1, 0 , 0), 33),
                      (new DateTime(2009, 01,  1, 0 , 0), 34),
                      (new DateTime(2012, 07,  1, 0 , 0), 35))


}

