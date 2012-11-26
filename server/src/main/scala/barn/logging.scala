package barn

object Logging {
  private val (logger, formatter) = ZeroLoggerFactory.newLogger(this)
}

trait Logging {

  import Logging.logger
  import Logging.formatter._

  def info(s: String) = logger.info(s)
  def warn(s: String) = logger.warning(s)
  def error(s: String) = logger.severe(s)

  def logBarnError(s: BarnError) = error(s.toString)

}

