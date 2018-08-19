import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "at.churchwood.",
      scalaVersion := "2.12.1",
      version      := "0"
    )),
    name := "MidiPascalCompiler",
    libraryDependencies += scalaTest % Test
  )
