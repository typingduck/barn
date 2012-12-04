package barn

import scalaz._
import Scalaz._

import org.apache.hadoop.conf.{Configuration => HadoopConf}
import scopt.immutable.OptionParser

object ParamParser extends ParamParser

trait ParamParser
  extends Logging {

  case class BarnConf(localLogDir : String
                    , hdfsLogDir : String
                    , hdfsEndpoint : String)

  val parser = new OptionParser[BarnConf]("barn-baler") {
    def options = Seq(
      opt("l", "local-log-dir", "<dir>", "directory containing the local logs.")
          {(v: String, c: BarnConf) => c.copy(localLogDir = v)},
      opt("h", "hdfs-log-dir", "<dir>", "directory containing shipped logs.")
          {(v: String, c: BarnConf) => c.copy(hdfsLogDir = v)},
      opt("s", "hdfs", "<hdfs://host:port>", "hdfs endpoint")
          {(v: String, c: BarnConf) => c.copy(hdfsEndpoint = v)}
    )
  }

  def loadConf(args: Array[String])
              (body: (HadoopConf, Dir, HdfsDir) => Unit)
  : Unit = {

    parser.parse(args, BarnConf(null,null,null)) map { config =>

      if(config.localLogDir == null ||
         config.hdfsLogDir == null ||
         config.hdfsEndpoint == null) {

        parser.showUsage
      } else {

        val hdfsEndpoint = tap(new HadoopConf()){_.set("fs.default.name"
                                                          , config.hdfsEndpoint)}
        val localLogDir = new Dir(config.localLogDir)
        val hdfsLogDir = new HdfsDir(config.hdfsLogDir)

        body(hdfsEndpoint, localLogDir, hdfsLogDir)
      }
    } getOrElse { false }
  }



}

