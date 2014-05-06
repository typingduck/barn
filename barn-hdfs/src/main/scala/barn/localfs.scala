package barn

object LocalFS extends LocalFS

trait LocalFS {

  import scalaz._
  import Scalaz._

  def listSubdirectories(dir: Dir)
  : Validation[BarnError, List[Dir]]
  = validate(dir.listFiles.toList.filter(x => x.isDirectory) success,
    "Can't get list of local services directories on: " + dir)

  def sumFileSizes(files: List[File]) : Long = {
    files.map(_.length).sum
  }

  def listSortedLocalFiles(dir : Dir, exclude: List[String] = List.empty)
  : Validation[BarnError, List[File]] = validate ({
    dir.listFiles
       .toList
       .filterNot(_.isDirectory)
       .filterNot(x => exclude.foldLeft(false) {
          (res, pattern) => res || x.getName.matches(pattern) })
       .sorted
       .success}
  , "Can't list local files on: " + dir)

}
