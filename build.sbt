ThisBuild / scalaVersion := "2.13.3"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "io.hades"

val circeVersion = "0.12.3"

lazy val root = (project in file("."))
  .settings(
    name := "hades-lang"
  )
  .aggregate(
    languageServer
  )
lazy val commonSettings = Seq(
  scalaVersion := "2.13.3",
  scalacOptions ++= Seq(
    "-Ymacro-annotations",
    "-encoding", "utf8",
    "-Xfatal-warnings",
    "-deprecation",
    "-unchecked",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-language:postfixOps"
  ),
)
lazy val languageServer = project
  .in(file("language-server"))
  .settings(commonSettings)
  .settings(
    packMain := Map(
      "hades-language-server" -> "hades.languageserver.LanguageServerMain"
    )
  )
  .settings(
    name := "hades-language-server",
    libraryDependencies ++= Seq(
      "org.junit.jupiter" % "junit-jupiter-engine" % "5.2.0" % "test",
      "org.junit.jupiter" % "junit-jupiter-api" % "5.3.1" % "test",
      "org.typelevel" %% "cats-core" % "2.2.0",
      "org.typelevel" %% "cats-effect" % "2.2.0",
    ),

    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion)
  )
  .enablePlugins(PackPlugin)

