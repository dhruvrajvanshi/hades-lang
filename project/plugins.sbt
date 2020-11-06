addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.13")

addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
)
