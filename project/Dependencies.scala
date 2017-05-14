import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1"
  lazy val scalactic = "org.scalactic" %% "scalactic" % "3.0.1"
  lazy val scalaMock = "org.scalamock" %% "scalamock-scalatest-support" % "3.5.0" % Test
  lazy val slack = "com.github.gilbertw1" %% "slack-scala-client" % "0.2.1"
}
