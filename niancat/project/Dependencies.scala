import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  lazy val scalactic = "org.scalactic" %% "scalactic" % "3.0.1"
  lazy val scalaMock = "org.scalamock" %% "scalamock-scalatest-support" % "3.5.0" % "test"
}
