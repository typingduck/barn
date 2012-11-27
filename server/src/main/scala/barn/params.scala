package barn

import scalaz._
import Scalaz._

import org.apache.hadoop.conf.{Configuration => HadoopConf}
import scopt.immutable.OptionParser

object ParamParser extends ParamParser

trait ParamParser
  extends Logging {

  case class BarnConf(rootLogDir : String
                    , rootHdfsDir : String
                    , hdfsEndpoint : String)

  val parser = new OptionParser[BarnConf]("barn-server") {
    def options = Seq(
      opt("l", "root-log-dir", "<dir>", "directory containing the local logs.")
          {(v: String, c: BarnConf) => c.copy(rootLogDir = v)},
      opt("h", "root-hdfs-dir", "<dir>", "directory containing shipped logs.")
          {(v: String, c: BarnConf) => c.copy(rootHdfsDir = v)},
      opt("s", "hdfs", "<hdfs://host:port>", "hdfs endpoint")
          {(v: String, c: BarnConf) => c.copy(hdfsEndpoint = v)}
    )
  }

  def loadConf(args: Array[String])
              (body: (HadoopConf, Dir, HdfsDir) => Unit)
  : Unit = {

    parser.parse(args, BarnConf(null,null,null)) map { config =>

      if(config.rootLogDir == null ||
         config.rootHdfsDir == null ||
         config.hdfsEndpoint == null) {

        parser.showUsage
      } else {

        val hdfsEndpoint = tap(new HadoopConf()){_.set("fs.default.name"
                                                          , config.hdfsEndpoint)}
        val rootLogDir = new Dir(config.rootLogDir)
        val rootHdfsDir = new HdfsDir(config.rootHdfsDir)

        body(hdfsEndpoint, rootLogDir, rootHdfsDir)
      }
    } getOrElse { false }
  }



}

