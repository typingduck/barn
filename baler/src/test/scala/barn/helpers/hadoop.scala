package barn

object HadoopTestHelpers {

  import barn._

  import org.apache.hadoop.fs.{FileSystem => HdfsFileSystem}

  def havingTemporaryDirStructure[T](fs: HdfsFileSystem,
                                     base: HdfsDir,
                                     files: List[HdfsFile])
                                    (f: => T) : T = {
    fs.delete(base, true)
    fs.mkdirs(base)

    files.foreach(fs.create(_).close)

    tap(f) { _ => fs.delete(base, true) }
  }

  def havingTemporaryDirStructures[T](fs: HdfsFileSystem,
                                      dirs: List[(HdfsDir,List[HdfsFile])])
                                     (f: => T) : T = {
    dirs.foreach { case (base, files) =>
      fs.delete(base, true)
      fs.mkdirs(base)
      files.foreach(fs.create(_).close)
    }

    tap(f) { _ =>
      dirs.foreach { case (base, files) =>
        fs.delete(base, true)
      }
    }
  }

}
