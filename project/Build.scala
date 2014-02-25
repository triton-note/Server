import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "TritonNote-Server"
  val appVersion      = "0.1.0-SNAPSHOT"

  val appDependencies = Seq(
    "net.databinder.dispatch" %% "dispatch-core" % "0.10.1",
    "securesocial" %% "securesocial" % "master-SNAPSHOT" withSources() exclude("org.scala-stm", "scala-stm_2.10.0"),
    "com.amazonaws" % "aws-java-sdk" % "1.7.1" withSources,
    "org.scalaz" %% "scalaz-core" % "7.0.5" withSources,
    jdbc,
    anorm
  )

  scalaVersion := "2.10.3"

  val main = play.Project(appName, appVersion, appDependencies).settings(
    resolvers ++= Seq(
      Resolver.url("sbt-plugin-snapshots", new URL("http://repo.scala-sbt.org/scalasbt/sbt-plugin-snapshots/"))(Resolver.ivyStylePatterns),
      Resolver.url("sbt-plugin-releases", new URL("http://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)
    )
  )

}
