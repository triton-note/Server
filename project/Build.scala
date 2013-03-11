import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "TritonNote-Server"
  val appVersion      = "0.1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    "postgresql" % "postgresql" % "9.1-901-1.jdbc4",
    "com.typesafe.slick" %% "slick" % "1.0.0",
    "postgresql" % "postgresql" % "9.1-901.jdbc4",
    jdbc,
    anorm
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
