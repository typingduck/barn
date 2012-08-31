import AssemblyKeys._

name := "barn-hdfs-writer"

version := "0.1"

scalaVersion := "2.9.2"

resolvers += "SC Hosted" at "http://maven.int.s-cloud.net/content/groups/hosted"

resolvers += "SC Proxy"  at "http://maven.int.s-cloud.net/content/groups/proxy"

libraryDependencies += "joda-time" % "joda-time" % "2.0"

libraryDependencies += "org.joda" % "joda-convert" % "1.1"

libraryDependencies += "org.apache.hadoop" % "hadoop-core" % "0.20.2-cdh3u1"

libraryDependencies += "commons-io" % "commons-io" % "2.4"

libraryDependencies += "commons-lang" % "commons-lang" % "2.4"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.4"

libraryDependencies += "com.dongxiguo" %% "zero-log" % "0.1.2"

assemblySettings

excludedJars in assembly <<= (fullClasspath in assembly) map { x =>
  val excluded = Seq("servlet-api", "jsp-api")
  x filter (jar => excluded.exists(jar.data.getName.startsWith))
}

