import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "io.github.dandeliondeathray",
      scalaVersion := "2.12.1",
      version      := "1.0rc1"
    )),
    name := "niancat",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalactic,
    libraryDependencies += scalaMock
  )
