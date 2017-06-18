package io.github.dandeliondeathray.niancat

import java.io.PrintWriter

import org.scalatest._

import scala.util.{Failure, Success}

class TokenSpec extends FlatSpec with Matchers {
  "a Token" should "read from a dot directory in the users home" in {
    val token = "01234567889abcdef"
    // Write the file $HOME/.slack/token_integration_test.token
    val home = System.getenv("HOME")
    assume(home != null)
    val slackDir = new java.io.File(s"$home/.slack")

    if (!slackDir.exists()) {
      assume(slackDir.mkdirs())
    }

    val tokenFile = new java.io.File(s"$home/.slack/token_integration_test.token")
    val pw = new PrintWriter(tokenFile, "UTF-8")
    pw.write(token)
    pw.close()

    // Supply the name token_integration_test and expect the token back
    Token.read("token_integration_test") shouldBe Success(token)
  }

  it should "return failure for an unknown token" in {
    Token.read("there_is_no_such_token") shouldBe a [Failure[_]]
  }
}