val scala3Version = "3.0.0-RC3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "algorithms",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalatest" % s"scalatest_${scala3Version}" % "3.2.8"
  )
