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

class PuzzleEngine(val dictionary: Dictionary,
                   val puzzleSolution: PuzzleSolution,
                   var puzzle: Option[Puzzle] = None) {
  def set(p: Puzzle): Response = {
    if (Some(p) == puzzle) {
      return SamePuzzle(p)
    }

    val noOfSolutions = puzzleSolution.noOfSolutions(p)
    if (noOfSolutions == 0) {
      return InvalidPuzzle(p)
    }

    puzzle = Some(p)

    val responses: Vector[Option[Response]] = Vector(
      Some(NewPuzzle { p }),
      puzzleSolution.result map (YesterdaysPuzzle(_)),
      Some(noOfSolutions) filter (_ > 1) map (MultipleSolutions(_))
    )

    puzzleSolution.reset(p)

    CompositeResponse(responses.flatten)
  }

  def get(): Response = {
    puzzle match {
      case None => NoPuzzleSet()
      case Some(p: Puzzle) => GetReply(p)
    }
  }

  def check(user: User, word: Word): Response = {
    puzzle match {
      case None => NoPuzzleSet()
      case Some(p: Puzzle) => checkSolution(user, word, p)
    }
  }

  private def checkSolution(user: User, word: Word, puzzle: Puzzle): Response = {
    import WritingSystemHelper._

    if (!(word isNineLetters)) {
      return IncorrectLength(word)
    }

    if (!matchWordAgainstPuzzle(word, puzzle)) {
      return WordAndPuzzleMismatch(word, puzzle)
    }
    if (dictionary has word) {
      puzzleSolution.solved(user, word)
      CorrectSolution(word)
    } else {
      NotInTheDictionary(word)
    }
  }

  private def matchWordAgainstPuzzle(word: Word, puzzle: Puzzle): Boolean = {
    word.letters.sorted == puzzle.letters.sorted
  }
}

trait Response
trait Reply extends Response
trait Notification extends Response

case class CompositeResponse(responses: Vector[Response]) extends Response
case class NoResponse() extends Response

case class NoPuzzleSet() extends Reply
case class GetReply(puzzle: Puzzle) extends Reply
case class NotInTheDictionary(word: Word) extends Reply
case class CorrectSolution(word: Word) extends Reply
case class WordAndPuzzleMismatch(word: Word, puzzle: Puzzle) extends Reply
case class IncorrectLength(word: Word) extends Reply
case class InvalidPuzzle(puzzle: Puzzle) extends Reply
case class InvalidCommandReply(msg: String, invalidCommandError: InvalidCommandError) extends Reply
case class SamePuzzle(puzzle: Puzzle) extends Reply

case class NewPuzzle(puzzle: Puzzle) extends Notification
case class YesterdaysPuzzle(result: SolutionResult) extends Notification
case class MultipleSolutions(n: Int) extends Notification