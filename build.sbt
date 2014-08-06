name := """TritonNote-Server"""

version := "0.4.1-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "com.amazonaws" % "aws-java-sdk" % "1.8.5",
  "org.scalaz" %% "scalaz-core" % "7.0.6",
  "com.sksamuel.scrimage" %% "scrimage-canvas" % "1.4.1",
  "com.google.apis" % "google-api-services-plus" % "v1-rev137-1.19.0",
  ws
)
