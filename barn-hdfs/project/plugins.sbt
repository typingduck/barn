resolvers ++= Seq(
  Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)
, "SC Proxy"  at "http://maven.int.s-cloud.net/content/groups/proxy"
)

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.11.2")

//addSbtPlugin("com.twitter" % "sbt-package-dist" % "1.0.7")
