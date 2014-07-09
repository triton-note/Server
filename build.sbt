name := """TritonNote-Server"""

version := "0.3"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  "com.amazonaws" % "aws-java-sdk" % "1.7.13" withSources,
  "org.scalaz" %% "scalaz-core" % "7.0.6" withSources,
  "com.sksamuel.scrimage" %% "scrimage-canvas" % "1.4.1" withSources,
  ws
)
