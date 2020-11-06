ThisBuild / scalaVersion := "2.13.3"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "io.hades"

lazy val root = (project in file("."))
  .settings(
    name := "hades-lang"
  )
  .settings(commonSettings)
  .aggregate(
    languageServer
  )
lazy val commonSettings = Seq(
  version := "0.0.1",
  scalaVersion := "2.13.3",
  libraryDependencies ++= Seq(
    "org.junit.jupiter" % "junit-jupiter-engine" % "5.2.0" % "test",
    "org.junit.jupiter" % "junit-jupiter-api" % "5.3.1" % "test",
  ),
)
lazy val languageServer = project
  .in(file("language-server"))
  .settings(commonSettings)
  .settings(
    name := "hades-language-server"
  )
