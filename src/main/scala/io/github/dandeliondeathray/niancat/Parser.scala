package io.github.dandeliondeathray.niancat

/** A PuzzleCommand is a command sent by a user to Niancat. */
sealed trait PuzzleCommand {
  def apply(engine: PuzzleEngine): Response
}

case class SetPuzzle(puzzle: Puzzle) extends PuzzleCommand {
  def apply(engine: PuzzleEngine): Response = {
    engine.set(puzzle)
  }
}

case class Get() extends PuzzleCommand {
  def apply(engine: PuzzleEngine): Response = engine.get()
}

case class CheckSolution(word: Word, user: User) extends PuzzleCommand {
  def apply(engine: PuzzleEngine): Response = {
    engine.check(user, word)
  }
}

case class Ignored() extends PuzzleCommand {
  def apply(engine: PuzzleEngine): Response = NoResponse()
}

sealed trait InvalidCommandError
case class UnknownCommand(command: String) extends InvalidCommandError
case class WrongArguments(actualNoOfArgs: Int, expectedNoOfArgs: Int) extends InvalidCommandError

case class InvalidCommand(msg: String, errorType: InvalidCommandError) extends PuzzleCommand {
  def apply(engine: PuzzleEngine): Response = InvalidCommandReply(msg, errorType)
}

case class AddUnsolution(unsolution: String, user: User) extends PuzzleCommand {
  def apply(engine: PuzzleEngine): Response = engine.addUnsolution(unsolution, user)
}
case class ListUnsolutions(user: User) extends PuzzleCommand {
  def apply(engine: PuzzleEngine): Response = engine.listUnsolutions(user)
}

sealed trait ChannelVisibility
case class PrivateChannel() extends ChannelVisibility
case class PublicChannel() extends ChannelVisibility

trait Parser {
  def parse(msg: String, user: User, visibility: ChannelVisibility): PuzzleCommand
}

class SlackParser extends Parser {
  def parse(msg: String, user: User, visibility: ChannelVisibility): PuzzleCommand = {
    if (!msg.startsWith("!")) {
      if (visibility == PrivateChannel()) return CheckSolution(Word(msg), user)

      return Ignored()
    }
    val words = msg.split(" ", 2)

    if (words(0) == "!nian") {
      return Get()
    }

    if (words(0) == "!setnian") {
      if (words.size != 2) return InvalidCommand(msg, WrongArguments(words.size - 1, 1))
      return SetPuzzle(Puzzle(words(1)))
    }

    if (words(0) == "!olösning") {
      if (words.size != 2) return InvalidCommand(msg, WrongArguments(words.size - 1, 1))
      return AddUnsolution(words(1), user)
    }

    if (words(0) == "!olösningar") {
      return if (visibility == PrivateChannel()) ListUnsolutions(user) else Ignored()
    }

    if (visibility == PublicChannel()) {
      return Ignored()
    } else {
      return InvalidCommand(msg, UnknownCommand(words(0)))
    }
  }
}
