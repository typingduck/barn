package barn

object FileTestHelpers {

  import java.io.File
  import barn._

  def havingTemporaryDirStructure[T](base: File, subs: List[File], files: List[File])
                                          (f: => T) : T = {
    import org.apache.commons.io.FileUtils
    FileUtils.deleteDirectory(base)

    base.mkdir
    subs.foreach(_.mkdir)
    files.foreach(_.createNewFile)

    tap(f) { _ =>
      files.foreach(_.delete)
      subs.foreach(_.delete)
      FileUtils.deleteDirectory(base)
    }
  }

}
