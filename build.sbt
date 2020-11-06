import sbt._
val languageVersion = "2.13.3"

lazy val core = project
  .in(file("hades-core"))
  .settings(commonSettings)
  .settings(
    name := "hades-core"
  )

// PROJECTS
lazy val global = project
  .in(file("."))
  .aggregate(
    languageServer,
  )


lazy val commonSettings = Seq(
  version := "0.0.1",
  scalaVersion := scalaVersion,
  libraryDependencies ++= Seq(
    //    "com.novocode" % "junit-interface" % "0.11" % "test",
    "org.junit.jupiter" % "junit-jupiter-engine" % "5.2.0" % "test",
    "org.junit.jupiter" % "junit-jupiter-api" % "5.3.1" % "test",
  ),
)
