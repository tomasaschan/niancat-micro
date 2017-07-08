import Dependencies._

val Http4sVersion = "0.15.11a"

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
    libraryDependencies += scalaMock,
    libraryDependencies ++= Seq(
      "org.http4s"     %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s"     %% "http4s-circe"        % Http4sVersion,
      "org.http4s"     %% "http4s-dsl"          % Http4sVersion,
      "ch.qos.logback" %  "logback-classic"     % "1.2.1"
    )
  )
