package object barn {

  import java.io.{File => File_}
  import org.apache.hadoop.fs.Path
  import scala.util.control.Exception._
  import org.apache.commons.lang.exception.ExceptionUtils._
  import scalaz._
  import Scalaz._
  import org.apache.hadoop.conf.{Configuration => HadoopConf}

  /*
    File/Dir/Path on Local/Hdfs legend for the confusionary situation:

    type Dir -> local directory
    type File -> local File
    type HdfsDir -> hdfs directory
    type HdfsFile -> hdfs file

    the word "path is intentionally avoided as much as possible.
  */
  type Dir = File_
  type File = File_
  type HdfsFile = Path
  type HdfsDir = HdfsFile

  sealed trait BarnError
  sealed trait BarnFatalError extends BarnError

  case class ThrownException(str: String) extends BarnFatalError
  case class RenameFailed(str: String) extends BarnFatalError
  case class InvalidNameFormat(str: String) extends BarnFatalError
  case class CombinedError(errors: BarnError*) extends BarnFatalError
  case class FileNotFound(str: String) extends BarnFatalError

  case class NothingToSync(str: String) extends BarnError
  case class SyncThrottled(str: String) extends BarnError

  case class BarnConf(localLogDir: Dir
                    , localTempDir: Dir
                    , hdfsLogDir: HdfsDir
                    , hdfsEndpoint: HadoopConf
                    , runParallel: Boolean
                    , degParallel: Integer
                    , shipInterval: Int
                    , gangliaHost: String
                    , gangliaPort: Int
                    , appName : String = "barn-hdfs")

  private val lineDelim = System.getProperty("line.separator")

  import java.util.concurrent.ConcurrentHashMap
  import scala.collection._

  case class PlacedFileInfo(bucket   : DateBucket
                         , host     : String
                         , service  : String
                         , taistamp : String)

  case class DateBucket(year: Int,
                        month: Int,
                        day: Int)

  type HdfsListCacheJ = ConcurrentHashMap[HdfsDir, \/[BarnError, BarnError \/ List[PlacedFileInfo]]]
  type HdfsListCache = concurrent.Map[HdfsDir, \/[BarnError, \/[BarnError, List[PlacedFileInfo]]]]

  def validate[U](body: => BarnError \/ U,
                  detail: String = null,
                  carryException: Boolean = true)
  : BarnError \/ U
  = allCatch either body fold ( exception => ThrownException(
    detail match {
      case null => getStackTrace(exception)
      case sth if carryException => (detail + lineDelim + getStackTrace(exception))
      case sth if !carryException => (detail)
    }).left , identity)

  def tap[A](a: A)(f: A => Unit) : A = {f(a); a}

  object -> {
    def unapply[A, B](pair: (A, B)): Option[(A, B)] =
      Some(pair)
  }

  implicit def errorConcat(a:String, b:String) = a + " and " + b
  implicit def errorConcat(a:BarnError, b:BarnError) = CombinedError(a,b)

  def collapseValidate[A, B](v: List[A \/ B])
                            (implicit op : (A,A) => A)
  : A \/ List[B]= {
    val (errors, values) = v.foldLeft((List.empty[A], List.empty[B])) {
      case ((errors, vals), el) =>
        el.fold(x => ( errors :+ x , vals), x => (errors, vals :+ x) )
    }

    errors match {
      case Nil => values.right
      case head::tail => tail.fold(head)(op).left
    }
  }

  class LazyWrapper[T](wrp: => T) {
    lazy val wrapped: T = wrp
  }

  object LazyWrapper {
    implicit def unboxLazy[T](wrapper: LazyWrapper[T]): T = wrapper.wrapped
  }

}
