name := """TritonNote-Server"""

version := "0.6.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.4"

scalacOptions ++= Seq(
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:reflectiveCalls",
  "-feature"
)

libraryDependencies ++= Seq(
  "com.amazonaws" % "aws-java-sdk" % "1.9.4",
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "com.sksamuel.scrimage" %% "scrimage-canvas" % "1.4.2",
  "com.google.apis" % "google-api-services-plus" % "v1-rev193-1.19.0",
  ws
)
