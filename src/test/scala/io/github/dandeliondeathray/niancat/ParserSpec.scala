package io.github.dandeliondeathray.niancat

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  "a Parser" should "create a get command from !nian" in {
    val parser = new SlackParser()

    val command = parser.parse("!nian", User("whatever"), PublicChannel())

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

  it should "respond with invalid command if it's unrecognized in private" in {
    val parser = new SlackParser()

    val command = parser.parse("!notacommand", User("foo"), PrivateChannel())

    command shouldBe InvalidCommand("!notacommand", UnknownCommand("!notacommand"))
  }

  it should "return invalid command, wrong no of args when !setnian is called with zero args" in {
    val parser = new SlackParser()

    val command = parser.parse("!setnian", User("foo"), PublicChannel())

    val gotArgs = 0
    val expectedArgs = 1
    command shouldBe InvalidCommand("!setnian", WrongArguments(gotArgs, expectedArgs))
  }

  it should "ignore commands it doesn't understand when in public" in {
    val parser = new SlackParser()

    val command = parser.parse("!notacommand", User("foo"), PublicChannel())

    command shouldBe Ignored()
  }

  it should "make an AddUnsolution for !olösning in public" in {
    val parser = new SlackParser()

    val command = parser.parse("!olösning ABC DEF", User("foo"), PublicChannel())

    command shouldBe AddUnsolution("ABC DEF", User("foo"))
  }

  it should "make AddUnsolution in private as well" in {
    val parser = new SlackParser()

    val command = parser.parse("!olösning ABC DEF", User("foo"), PublicChannel())

    command shouldBe AddUnsolution("ABC DEF", User("foo"))
  }

  it should "require an argument to !olösning" in {
    val parser = new SlackParser()

    val command = parser.parse("!olösning", User("foo"), PrivateChannel())

    val gotArgs = 0
    val expectedArgs = 1
    command shouldBe InvalidCommand("!olösning", WrongArguments(gotArgs, expectedArgs))
  }

  it should "make ListUnsolutions from !olösningar in private" in {
    val parser = new SlackParser()

    val command = parser.parse("!olösningar", User("foo"), PrivateChannel())

    command shouldBe ListUnsolutions(User("foo"))
  }

  it should "ignore !olösningar in public" in {
    val parser = new SlackParser()

    val command = parser.parse("!olösningar", User("foo"), PublicChannel())

    command shouldBe Ignored()
  }
}