package io.github.dandeliondeathray.niancat

import org.scalactic._
import NormMethods._
import java.text.Normalizer

object StringNormalizer {
  implicit val stringNormalizer = new Normalization[String] {
    def normalized(s: String): String = {
      val decomposed = Normalizer.normalize(s, Normalizer.Form.NFKD).replaceAll("[\u0301\u0341]", "").toUpperCase
      Normalizer.normalize(decomposed, Normalizer.Form.NFKC)
    }
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
case class Puzzle(letters: String)

/** A Word is a potential solution to a Puzzle, but can be any length. */
case class Word(letters: String) {
  def matches(p: Puzzle): Boolean = {
    letters.norm.sorted == p.letters.norm.sorted
  }
}

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

    if (!(word matches puzzle)) {
      return WordAndPuzzleMismatch(word, puzzle)
    }
    if (dictionary has word) {
      puzzleSolution.solved(user, word)
      CorrectSolution(word)
    } else {
      NotInTheDictionary(word)
    }
  }
}

