package io.github.dandeliondeathray.niancat

import org.scalactic._
import NormMethods._
import java.text.Normalizer
import scala.collection.mutable

object StringNormalizer {
  implicit val stringNormalizer = new Normalization[String] {
    def normalized(s: String): String = {
      val decomposed = Normalizer.normalize(s, Normalizer.Form.NFKD).replaceAll("[- _\u0301\u0341]", "").toUpperCase
      Normalizer.normalize(decomposed, Normalizer.Form.NFKC)
    }
  }
  implicit val puzzleNormalizer = new Normalization[Puzzle] {
    def normalized(puzzle: Puzzle): Puzzle = Puzzle(puzzle.letters.norm)
  }
}
import StringNormalizer._

object WordNormalizer {
  implicit val wordNormalizer = new Normalization[Word] {
    def normalized(word: Word): Word = {
      Word(stringNormalizer.normalized(word.letters))
    }
  }
}

/**
  * Created by Erik Edin on 2017-04-30.
  */
/** A Puzzle is a String of exactly nine characters. */
case class Puzzle(letters: String) {
  def matches(p: Puzzle): Boolean = {
    letters.norm.sorted == p.letters.norm.sorted
  }
}

/** A Word is a potential solution to a Puzzle, but can be any length. */
case class Word(letters: String) {
  def matches(p: Puzzle): Boolean = {
    letters.norm.sorted == p.letters.norm.sorted
  }
}

/** A User is of course a user in the chat room. */
case class User(name: String)

class PuzzleEngine(val dictionary: Dictionary, val puzzleSolution: PuzzleSolution, var puzzle: Option[Puzzle] = None) {

  val unconfirmedUnsolutions: mutable.Map[User, String] = mutable.Map()
  val unsolutions: mutable.Map[User, List[String]] = mutable.Map()

  def set(nonNormalizedPuzzle: Puzzle, isWeekday: Boolean): Response = {
    val p = nonNormalizedPuzzle.norm
    if (puzzle.map(_ matches p) getOrElse false) {
      return SamePuzzle(p)
    }

    val noOfSolutions = puzzleSolution.noOfSolutions(p)
    if (noOfSolutions == 0) {
      return InvalidPuzzle(p)
    }

    puzzle = Some(p)

    val orderedUnsolutions = unsolutions.toMap mapValues (_ reverse)
    val maybeUnsolutions: Option[Map[User, List[String]]] =
      if (orderedUnsolutions.isEmpty) None else Some(orderedUnsolutions)

    val responses: Vector[Option[Response]] = Vector(
      Some(NewPuzzle { p }),
      puzzleSolution.result map (YesterdaysPuzzle(_)),
      Some(noOfSolutions) filter (_ > 1) map (MultipleSolutions(_)),
      maybeUnsolutions map (AllUnsolutions(_))
    )

    puzzleSolution.reset(p, isWeekday)
    unsolutions.clear()
    unconfirmedUnsolutions.clear()

    CompositeResponse(responses.flatten)
  }

  def get(): Response = {
    puzzle match {
      case None            => NoPuzzleSet()
      case Some(p: Puzzle) => GetReply(p)
    }
  }

  def check(user: User, word: Word, isWeekday: Boolean): Response = {
    puzzle match {
      case None            => NoPuzzleSet()
      case Some(p: Puzzle) => checkSolution(user, word, p, isWeekday)
    }
  }

  def addUnsolution(unsolution: String, user: User): Response = {
    if (puzzle.isEmpty) {
      return NoPuzzleSet()
    }

    if (!anyWordMatchesPuzzle(puzzle.get, unsolution)) {
      val unconfirmedUnsolution = unconfirmedUnsolutions.get(user)
      unconfirmedUnsolution match {
        case None if unsolution == "" => {
          // The user used an empty add unsolution command, but there's no unconfirmed unsolution.
          return NoUnsolutionToConfirm()
        }
        case None => {
          // The user added an unsolution, but no words matches the puzzle. There is no previous unconfirmed
          // unsolution.
          unconfirmedUnsolutions(user) = unsolution
          return UnsolutionNeedsConfirmation(puzzle.get)
        }
        case Some(text) if text != unsolution && unsolution != "" => {
          // The user added an unsolution, but no words matches the puzzle. There _is_ a previous unconfirmed
          // unsolution. We're overwriting it.
          unconfirmedUnsolutions(user) = unsolution
          return UnsolutionNeedsConfirmation(puzzle.get)
        }
        case Some(text) if unsolution == "" => {
          // The user confirmed an unsolution using the empty add unsolution command. Store the previously saved
          // unconfirmed unsolution.
          storeUnsolution(text, user)
          return UnsolutionAdded()
        }
        case default => {
          // In this case the user confirmed an unsolution by repeating it.
        }
      }
    }

    storeUnsolution(unsolution, user)
    UnsolutionAdded()
  }

  def listUnsolutions(user: User): Response = {
    val unsolutionsForUser = unsolutions.get(user)

    unsolutionsForUser match {
      case Some(texts) => Unsolutions(texts reverse)
      case None        => NoUnsolutions()
    }
  }

  private def storeUnsolution(unsolution: String, user: User) = {
    val unsolutionsForUser: List[String] = unsolutions.getOrElse(user, List[String]())
    unsolutions(user) = unsolution :: unsolutionsForUser
    unconfirmedUnsolutions.remove(user)
  }

  private def checkSolution(user: User, word: Word, puzzle: Puzzle, isWeekday: Boolean): Response = {
    import WritingSystemHelper._

    if (!(word isNineLetters)) {
      val tooMany: Option[String] = Some(word.letters.norm diff puzzle.letters.norm) filter (s => !s.isEmpty)
      val tooFew: Option[String] = Some(puzzle.letters.norm diff word.letters.norm) filter (s => !s.isEmpty)
      return IncorrectLength(word, tooMany, tooFew)
    }

    if (!(word matches puzzle)) {
      val tooMany = word.letters.norm diff puzzle.letters.norm
      val tooFew = puzzle.letters.norm diff word.letters.norm
      return WordAndPuzzleMismatch(word, puzzle, tooMany, tooFew)
    }
    if (dictionary has word) {
      puzzleSolution.solved(user, word, isWeekday)
      val noOfSolutions = puzzleSolution.noOfSolutions(puzzle)
      val solutionId: Option[Int] = puzzleSolution.solutionId(word) filter (_ => noOfSolutions > 1)
      if (!puzzleSolution.hasSolved(user, word))
        CompositeResponse(
          Vector(CorrectSolution(word), SolutionNotification(user, 1 + puzzleSolution.streak(user), solutionId))
        )
      else
        CorrectSolution(word)
    } else {
      NotInTheDictionary(word)
    }
  }

  private def anyWordMatchesPuzzle(puzzle: Puzzle, unsolution: String): Boolean = {
    val words = unsolution.split(" ") map (Word(_))
    words exists (_ matches puzzle)
  }
}
