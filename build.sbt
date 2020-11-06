ThisBuild / scalaVersion := "2.13.3"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "io.hades"

Global / onChangedBuildSource := ReloadOnSourceChanges

val circeVersion = "0.12.3"
val catsVersion  = "2.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "hades-lang"
  )
  .aggregate(
    languageServer
  )
lazy val commonSettings = Seq(
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
    ),

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core",
      "org.typelevel" %% "cats-effect",
    ).map(_ % catsVersion),

    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
  )
  .enablePlugins(PackPlugin)

