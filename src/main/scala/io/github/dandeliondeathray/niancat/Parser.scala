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

//case class AddUnsolution(unsolution: String, user: User) extends PuzzleCommand
//case class ListUnsolutions(user: User) extends PuzzleCommand

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

    if (words(0) == "!get") {
      return Get()
    }

    if (words(0) == "!setnian") {
      return SetPuzzle(Puzzle(words(1)))
    }

    Get()
  }
}
