package io.github.dandeliondeathray.niancat

/**
  * Created by Erik Edin on 2017-04-30.
  */

/** A Puzzle is a String of exactly nine characters. */
case class Puzzle(letters: String)

/** A Word is a potential solution to a Puzzle, but can be any length. */
case class Word(letters: String)

/** A User is of course a user in the chat room. */
case class User(name: String)

class PuzzleEngine(val dictionary: Dictionary, var puzzle: Option[Puzzle] = None) {
  def set(p: Puzzle): Response = {
    puzzle = Some(p)

    CompositeResponse(Vector(NewPuzzle {
      p
    }))
  }

  def get(): Response = {
    puzzle match {
      case None => NoPuzzleSet()
      case Some(p: Puzzle) => GetReply(p)
    }
  }

  def check(word: Word): Response = {
    puzzle match {
      case None => NoPuzzleSet()
      case Some(p: Puzzle) => checkSolution(word, p)
    }
  }

  private def checkSolution(word: Word, puzzle: Puzzle): Response = {
    import WritingSystemHelper._

    if (!(word isNineLetters)) {
      return IncorrectLength(word)
    }

    if (!matchWordAgainstPuzzle(word, puzzle)) {
      return WordAndPuzzleMismatch(word, puzzle)
    }
    if (dictionary has word) {
      CorrectSolution(word)
    } else {
      NotInTheDictionary(word)
    }
  }

  private def matchWordAgainstPuzzle(word: Word, puzzle: Puzzle): Boolean = {
    word.letters.sorted == puzzle.letters.sorted
  }
}

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
    engine.check(word)
  }
}
//case class AddUnsolution(unsolution: String, user: User) extends PuzzleCommand
//case class ListUnsolutions(user: User) extends PuzzleCommand

trait Response
trait Reply extends Response
trait Notification extends Response

case class CompositeResponse(responses: Vector[Response]) extends Response

case class NoPuzzleSet() extends Reply
case class GetReply(puzzle: Puzzle) extends Reply
case class NotInTheDictionary(word: Word) extends Reply
case class CorrectSolution(word: Word) extends Reply
case class WordAndPuzzleMismatch(word: Word, puzzle: Puzzle) extends Reply
case class IncorrectLength(word: Word) extends Reply

case class NewPuzzle(puzzle: Puzzle) extends Notification

