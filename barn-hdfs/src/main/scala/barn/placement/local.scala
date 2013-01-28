package barn.placement

import org.joda.time._
import scalaz._
import Scalaz._
import scala.math.Ordering
import barn._
import TimeUtils._

trait LocalPlacementStrategy
  extends Logging
  with SvlogdFile
  with LocalFS {

  val delim = '@'

  def decodeServiceInfo(serviceDir: Dir)
  : Validation[BarnError, LocalServiceInfo]
  = serviceDir.getName.split(delim) match {
    case Array(service, category, host) =>
      LocalServiceInfo(service, category, host) success
    case _ => InvalidNameFormat("Failed to extract service info for " + serviceDir) fail
  }

  def cleanupLocal(dir: Dir,
                   cleanupLimit: DateTime,
                   minMB: Int,
                   exclude: List[String] = List empty)
  : Validation[BarnError, Unit]
  = for {
      localFiles <- listSortedLocalFiles(dir, exclude)
      deletion   <- validate({

        //Sum of the size of all files subject to deletion
        val sumSize = localFiles.foldLeft(0L) { (sum, f) => sum + f.length}

        localFiles.foldLeft((0, sumSize)) {
          case deletedSoFar -> curSize -> file =>
            val ts = Tai64.convertTai64ToTime(svlogdFileNameToTaiString(file.getName))

            ts.isBefore(cleanupLimit) && curSize > minMB*1024*1024 match {
              case true =>
                val fileLength = file.length
                file.delete
                (deletedSoFar + 1) -> (curSize - fileLength)
              case false => deletedSoFar -> curSize
            }
        }
      }.success, "Deletion of retained files failed.")
    } yield tap(deletion) (x => info (x._1 + " retained files deleted and " +
                                        x._2 / (1024 * 1024) + "MB remained" +
                                        " on " + dir ))
}

