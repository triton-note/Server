name := """TritonNote-Server"""

version := "0.8.1-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.4"

scalacOptions ++= Seq(
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:reflectiveCalls",
  "-feature"
)

libraryDependencies ++= Seq(
  "org.fathens" %% "astronomy" % "1.1.1",
  "com.amazonaws" % "aws-java-sdk" % "1.9.13",
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "com.sksamuel.scrimage" %% "scrimage-canvas" % "1.4.2",
  "com.google.apis" % "google-api-services-plus" % "v1-rev193-1.19.0",
  ws
)

// for LESS CSS
includeFilter in (Assets, LessKeys.less) := "*.less"

excludeFilter in (Assets, LessKeys.less) := "_*.less"

LessKeys.compress := true
