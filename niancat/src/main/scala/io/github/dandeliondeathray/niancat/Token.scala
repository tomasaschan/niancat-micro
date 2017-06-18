package io.github.dandeliondeathray.niancat

import scala.util.{Failure, Try}
import scala.io.Source

object Token {
  def read(tokenName: String): Try[String] = Try {
    val home = System.getenv("HOME")
    if (home == null) throw new RuntimeException("Missing HOME environment variable")
    val filename = s"$home/.slack/$tokenName.token"
    Source.fromFile(filename).mkString
  }
}