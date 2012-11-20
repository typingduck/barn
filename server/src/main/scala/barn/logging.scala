package barn

object Logging extends Logging

trait Logging {

  //private val (logger, formatter) = ZeroLoggerFactory.newLogger(this)

  //import formatter._

  def info(s: String) = println(s)//logger.info(s)
  def warn(s: String) = println(s)//logger.warning(s)
  def error(s: String) = println(s)//logger.severe(s)
}

