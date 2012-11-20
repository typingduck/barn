package object barn {

  import java.io.{File => File_}
  import org.apache.hadoop.fs.Path
  import scala.util.control.Exception._
  import org.apache.commons.lang.exception.ExceptionUtils._
  import scalaz._
  import Scalaz._

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

  private val lineDelim = System.getProperty("line.separator")

  def validate[U](body: => Validation[String,U],
                  detail: String = null,
                  carryException: Boolean = true)
  : Validation[String, U]
  = allCatch either body fold ( exception =>
    detail match {
      case null => getStackTrace(exception).fail
      case sth if carryException => (detail + lineDelim + getStackTrace(exception)).fail
      case sth if !carryException => (detail).fail
    } , identity)

  def tap[A](a: A)(f: A => Unit) : A = {f(a); a}

  object -> {
    def unapply[A, B](pair: (A, B)): Option[(A, B)] =
      Some(pair)
  }

  implicit def errorConcat(a:String, b:String) = a + " and " + b

  def collapseValidate[A, B](v: List[Validation[A,B]])
                            (implicit op : (A,A) => A)
  : Validation[A, List[B]]= {
    val (errors, values) = v.foldLeft((List.empty[A], List.empty[B])) {
      case ((errors, vals), el) =>
        el.fold(x => ( errors :+ x , vals), x => (errors, vals :+ x) )
    }

    errors match {
      case Nil => values.success
      case head::tail => tail.fold(head)(op).fail
    }
  }

  def inceptRight[A,B](v: Option[Validation[A,B]])
  : Validation[A, Option[B]]
  = v.fold(_.fold(_.fail,_.some.success), None.success)
}
