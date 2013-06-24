import AssemblyKeys._
import com.twitter.sbt._

name := "barn-hdfs"

version := "0.1.22-CDH4"

organization := "com.soundcloud"

// FIXME: twitter's sbt-package-dist is only available for sbt 0.11.x + scala
// 2.9.1
scalaVersion := "2.10.0"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimize")

resolvers := Seq(
  "SC Hosted" at "http://maven.int.s-cloud.net/content/groups/hosted"
, "SC Proxy"  at "http://maven.int.s-cloud.net/content/groups/proxy"
//, "sonatype-public" at "https://oss.sonatype.org/content/groups/public"
//, "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
//, "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies := Seq(
  "joda-time"         %  "joda-time"    % "2.0"
, "org.joda"          %  "joda-convert" % "1.1"
//, "org.apache.hadoop" %  "hadoop-core"  % "0.20.2-cdh3u1"
, "org.apache.hadoop" % "hadoop-client" % "2.0.0-mr1-cdh4.3.0"
, "commons-io"        %  "commons-io"   % "2.4"
, "commons-lang"      %  "commons-lang" % "2.4"
, "org.scalatest" % "scalatest_2.10" % "1.9.1"  % "test,slow"
, "org.scalacheck"    %% "scalacheck"   % "1.10.0" % "test,slow"
, "org.scalaz" % "scalaz-core_2.10" % "7.0.0-M8"
, "com.yammer.metrics" % "metrics-ganglia"  % "3.0.0.sc-SNAPSHOT"
, "com.yammer.metrics" % "metrics-scala_2.9.1"  % "3.0.0.sc-SNAPSHOT"
, "org.scalatest" % "scalatest_2.10" % "1.9.1"
, "com.github.scopt" % "scopt_2.10" % "2.1.0"
//, "com.twitter" % "scalding-core_2.9.2" % "0.8.4-cdh4.1.0-sc1.0.0"
, "cascading" % "cascading-core" % "2.0.1-cdh4.1.0"
, "cascading" % "cascading-local" % "2.0.1-cdh4.1.0"
, "cascading" % "cascading-hadoop" % "2.0.1-cdh4.1.0"
)


//
// settings
//

PackageDist.newSettings

GitProject.gitSettings

net.virtualvoid.sbt.graph.Plugin.graphSettings


//
//  test settings + options
//

testOptions in Test += Tests.Argument("-oDS")

testOptions in Slow += Tests.Argument("-oDS")

// will not work with sbt < 0.12
fork in Test := true

fork in Slow := true


//
// artifacts
//

crossPaths := false

// unfortunately, the below don't prevent including those jars in the
// package-dist
publishArtifact in Test := false

publishArtifact in (Compile, packageDoc) := false

publishArtifact in (Compile, packageSrc) := false
