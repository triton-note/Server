name := """TritonNote-Server"""

version := "0.4.1-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.2"

scalacOptions ++= Seq(
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:reflectiveCalls",
  "-feature"
)

libraryDependencies ++= Seq(
  "com.amazonaws" % "aws-java-sdk" % "1.8.9.1",
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "com.sksamuel.scrimage" %% "scrimage-canvas" % "1.4.1",
  "com.google.apis" % "google-api-services-plus" % "v1-rev145-1.19.0",
  ws
)
