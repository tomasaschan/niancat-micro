package io.github.dandeliondeathray.niancat

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  "a Parser" should "create a get command from !nian" in {
    val parser = new SlackParser()

    val command = parser.parse("!get", User("whatever"), PublicChannel())

    command shouldBe Get()
  }

  it should "ignore non-command messages in public channels" in {
    val parser = new SlackParser()

    val command = parser.parse("Not a command", User("foo"), PublicChannel())

    command shouldBe Ignored()
  }

  it should "make a CheckSolution command for non-command messages in private channels" in {
    val parser = new SlackParser()

    val msg = "Not a command"
    val command = parser.parse(msg, User("foo"), PrivateChannel())

    command shouldBe CheckSolution(Word(msg), User("foo"))
  }

  it should "make a SetPuzzle command for !setnian" in {
    val parser = new SlackParser()

    val command = parser.parse("!setnian ABCDEFGHI", User("foo"), PublicChannel())

    command shouldBe SetPuzzle(Puzzle("ABCDEFGHI"))
  }
}