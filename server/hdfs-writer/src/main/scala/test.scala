import java.io.File
import org.joda.time._
import java.math.BigInteger

object Tai64 {

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

  def convertTai64ToTime(hex : String) : DateTime = {
    val tai_int = java.lang.Long.parseLong(hex.substring(0,16), 16)
    val nano_int = java.lang.Long.parseLong(hex.substring(16, 24), 16)
    val seconds = tai_int - 4611686018427387904L
    System.out.println("nano_int" + nano_int)
    System.out.println("secs:" + seconds)

    val basedate = new DateTime(1970, 1, 1, 0, 0, 0, 0);
    val timedelta = new Duration(seconds * 1000 + nano_int / 1000000)
    System.out.println("seconds: " + seconds + " nano_int " + nano_int + " nano_int/1000000 " + nano_int / 1000000)

    System.out.println("delta:" + timedelta)

    val final_ = basedate.plus(timedelta)

    val extraSeconds : Int = conversion_table.reverse.find(_._1.isBefore(final_)).map(_._2).getOrElse(0)

    final_.plusSeconds(extraSeconds * -1)
  }

}


object SeriesCheck {

  def timeSeriesRegular(ts : List[DateTime]) : Boolean = {
    System.out.println(ts)
    false
  }
}


object BarnHdfsWriter extends App {
/*
  val logPath = args(0)
  val exclude = List(".gitignore", "target")

  val files = new File(logPath).listFiles.toList.map(_.getName).diff(exclude)

  val unexpectedFiles = files.filterNot(_.endsWith(".s"))

  System.out.println("We have " + unexpectedFiles.size + " unexpected files:" + unexpectedFiles )
*/
  System.out.println("hello you " + Tai64.convertTai64ToTime("4000000050116573099c6554"))

  val ts = List("40000000500fccde38b255ec",
               "40000000500fce0008486a2c",
               "40000000500fce0d05980b84",
               "40000000500fd4062674815c",
               "40000000500fd611225d0134",
               "40000000500fd7d522ff82c4",
               "40000000500fd7fe1793fce4",
               "40000000501007e7030c440c",
               "4000000050100d802ebc978c",
               "4000000050100d8c106958ec")

  System.out.println(SeriesCheck.timeSeriesRegular(ts.map(Tai64.convertTai64ToTime _)))

}
