name := """TritonNote-Server"""

version := "0.8.11-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.5"

scalacOptions ++= Seq(
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:reflectiveCalls",
  "-feature"
)

libraryDependencies ++= Seq(
  "org.fathens" %% "astronomy" % "1.1.3" excludeAll(ExclusionRule(organization = "org.specs2")),
  "com.drewnoakes" % "metadata-extractor" % "2.7.2",
  "com.amazonaws" % "aws-java-sdk" % "1.9.21",
  "org.scalaz" %% "scalaz-core" % "7.0.6",
  "com.sksamuel.scrimage" %% "scrimage-canvas" % "1.4.2",
  "org.specs2" %% "specs2-scalacheck" % "2.3.12" % "test",
  ws
)

// for LESS CSS
includeFilter in (Assets, LessKeys.less) := "*.less"

excludeFilter in (Assets, LessKeys.less) := "_*.less"

LessKeys.compress := true
