ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.7"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client3" %% "core" % "3.9.0",
  "org.jaudiotagger" % "jaudiotagger" % "3.0.1",
  "com.lihaoyi" %% "ujson" % "3.1.0"
)

lazy val root = (project in file("."))
  .settings(
    name := "Mp3Idv2Tagging"
  )
