import AssemblyKeys._
import com.twitter.sbt._

name := "barn-hdfs"

version := "0.1.36"

organization := "com.soundcloud"

// FIXME: twitter's sbt-package-dist is only available for sbt 0.11.x + scala
// 2.9.1
scalaVersion := "2.10.4"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimize")

resolvers := Seq(
   "Cloudera Repo" at "https://repository.cloudera.com/artifactory/cloudera-repos/"
 )

libraryDependencies := Seq(
  "joda-time"         %  "joda-time"    % "2.0"
, "org.joda"          %  "joda-convert" % "1.1"
//, "org.apache.hadoop" %  "hadoop-core"  % "0.20.2-cdh3u1"
, "org.apache.hadoop" % "hadoop-client" % "2.0.0-mr1-cdh4.3.0"
, "commons-io"        %  "commons-io"   % "2.4"
, "commons-lang"      %  "commons-lang" % "2.4"
, "org.scalatest"     % "scalatest_2.10"    % "2.1.0"  % "test,slow"
, "org.scalacheck"    % "scalacheck_2.10"   % "1.11.3" % "test,slow"
, "org.scalaz"        % "scalaz-core_2.10"  % "7.0.6"
, "com.codahale.metrics" % "metrics-ganglia"  % "3.0.2"
, "com.codahale.metrics" % "metrics-servlet"  % "3.0.2"
, "nl.grons"          % "metrics-scala_2.10"  % "3.0.2"
, "com.github.scopt"  % "scopt_2.10" % "2.1.0"
)


//
// settings
//

PackageDist.newSettings

GitProject.gitSettings


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
