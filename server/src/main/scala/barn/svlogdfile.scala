package barn

import scalaz._
import Scalaz._
import org.joda.time._

object SvlogdFile extends SvlogdFile

trait SvlogdFile {
  def svlogdFileTimestamp(svlogdFile: File)
  : Validation[BarnError, DateTime] =
    validate(Tai64.convertTai64ToTime(svlogdFileNameToTaiString(svlogdFile.getName)).success,
    "Couldn't extract timestamp from svlogd file")

  def svlogdFileNameToTaiString(svlogdFileName: String)
  : String = svlogdFileName.drop(1).dropRight(2)
}


