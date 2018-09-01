name := "checkx"
organization := "com.here.cme.poc"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.6"

val versions = {
  object Version {
  }
  Version
}

resolvers += "Artifactory Realm" at "https://artifactory.in.here.com/artifactory/here-olp-sit/"
resolvers += "Nexus" at "https://ncr.in.here.com/nexus/content/repositories/releases/"
resolvers += "Navteq internal" at "https://ncr.in.here.com/nexus/content/groups/public/"
//resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

//addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")

//val resolvers = List(
//  "NCR Public" at "https://ncr.in.here.com/nexus/repository/public",
//"NCR Public Releases" at "https://ncr.in.here.com/nexus/repository/releases-group",
//"Here OLP" at "https://artifactory.in.here.com/artifactory/here-olp-sit",
//"Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
//"Artifactory NCR 3D Releases" at "https://artifactory.in.here.com/artifactory/NCR_3D_Releases",
// Resolver.mavenLocal)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" withSources() withJavadoc()
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
