import AssemblyKeys._
import com.twitter.sbt._

name := "barn-server"

version := "0.1"

// FIXME: twitter's sbt-package-dist is only available for sbt 0.11.x + scala
// 2.9.1
scalaVersion := "2.9.1"

resolvers := Seq(
  "SC Hosted" at "http://maven.int.s-cloud.net/content/groups/hosted"
, "SC Proxy"  at "http://maven.int.s-cloud.net/content/groups/proxy"
, "sonatype-public" at "https://oss.sonatype.org/content/groups/public"
)

libraryDependencies := Seq(
  "joda-time" % "joda-time" % "2.0"
, "org.joda" % "joda-convert" % "1.1"
, "org.apache.hadoop" % "hadoop-core" % "0.20.2-cdh3u1"
, "commons-io" % "commons-io" % "2.4"
, "commons-lang" % "commons-lang" % "2.4"
, "org.scalaz" %% "scalaz-core" % "6.0.4"
, "org.scalatest" %% "scalatest" % "1.6.1" % "test"
, "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
, "com.github.scopt" %% "scopt" % "2.1.0"
)

PackageDist.newSettings

GitProject.gitSettings

// unfortunately, the below don't prevent including those jars in the
// package-dist
publishArtifact in Test := false

publishArtifact in (Compile, packageDoc) := false

publishArtifact in (Compile, packageSrc) := false
