lazy val advent2020 = (project in file("."))
  .settings (
    name := "Advent2020",
    organization := "org.saegesser",
    version := "0.0.0-SNAPSHOT",
    scalaVersion in ThisBuild := "2.13.4",
    scalacOptions in ThisBuild ++= Seq(
      "-feature",
      "-deprecation",
      // "-Yno-adapted-args",
      "-Ywarn-value-discard",
      "-Ywarn-numeric-widen",
      "-Ywarn-dead-code",
      "-Xlint",
      "-Xfatal-warnings",
      "-unchecked",
      "-language:implicitConversions"
    ),

    scalacOptions in (Compile, console) ~= (_.filterNot(_ == "-Xlint")),
    scalacOptions in (Test, console) ~= (_.filterNot(_ == "-Xlint")),

    initialCommands := """import advent._
"""
  )
