name := "sd-repo"

version := "0.1"

scalaVersion := "2.13.8"

libraryDependencies ++= List(
  "org.scalatest" %% "scalatest" % "3.2.11" % "test",
  "ch.qos.logback" % "logback-classic" % "1.2.10",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
  "com.typesafe.akka" %% "akka-stream" % "2.6.18"

)
