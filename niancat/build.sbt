import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "io.github.dandeliondeathray",
      scalaVersion := "2.12.1",
      version      := "1.0rc1"
    )),
    name := "niancat",
    exportJars := true,
    retrieveManaged := true,
    libraryDependencies += scalaTest,
    libraryDependencies += scalactic,
    libraryDependencies += scalaMock
  )
