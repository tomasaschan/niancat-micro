package io.github.dandeliondeathray.niancat

import org.scalactic.NormMethods._
import PuzzleNormalizer._
import WordNormalizer._

case class SolutionResult(wordsAndSolvers: Map[Word, Seq[User]] = Map(), streaks: Map[User, Int] = Map())

trait State {
  def puzzle: Option[Puzzle]
  def unsolutions: Map[User, List[String]]
  def unconfirmedUnsolutions: Map[User, String]

  def result(allSolutions: Seq[Word]): Option[SolutionResult]
  def reset(p: Puzzle, isWeekday: Boolean): Unit
  def solved(user: User, word: Word, isWeekday: Boolean): Unit
  def hasSolved(user: User, word: Word): Boolean
  def streak(user: User): Int

  def storeUnsolution(user: User, text: String)
  def storeUnconfirmedUnsolution(user: User, text: String)
}

class NiancatState extends State {
  var puzzle: Option[Puzzle] = None
  var unsolutions: Map[User, List[String]] = Map()
  var unconfirmedUnsolutions: Map[User, String] = Map()
  var solutions: Seq[(Word, User)] = Seq()
  var streaks: Map[User, Int] = Map()

  override def result(allSolutions: Seq[Word]): Option[SolutionResult] = {
    puzzle match {
      case None => None
      case Some(p) => {
        val allSolutionsMap: Map[Word, Seq[User]] = (allSolutions map (_ -> Seq[User]())).toMap
        val foundSolutions: Map[Word, Seq[User]] = (solutions groupBy (_._1) mapValues (_.map(_._2).toSeq))
        Some(SolutionResult(allSolutionsMap ++ foundSolutions, streaks.toMap))
      }
    }
  }

  override def reset(p: Puzzle, isWeekday: Boolean): Unit = {
    puzzle = Some(p.norm)
    if (isWeekday) {
      streaks = streaks filter { case (user, _) => solutions.map(_._2) contains user }
    }
    solutions = Seq()
    unsolutions = Map()
    unconfirmedUnsolutions = Map()
  }
  override def solved(user: User, word: Word, isWeekday: Boolean): Unit = {
    val userHasFoundThisSolutionBefore = solutions contains ((word.norm, user))
    if (!userHasFoundThisSolutionBefore && isWeekday) {
      streaks += user -> ((streaks getOrElse (user, 0)) + 1)
    }
    solutions :+= (word.norm, user)
  }

  override def hasSolved(user: User, word: Word): Boolean = {
    solutions contains ((word.norm, user))
  }
  override def streak(user: User): Int = streaks getOrElse (user, 0)

  override def storeUnsolution(user: User, text: String): Unit = {
    val unsolutionsForUser: List[String] = unsolutions.getOrElse(user, List[String]())
    unsolutions += (user -> (text :: unsolutionsForUser))
    unconfirmedUnsolutions -= user
  }

  override def storeUnconfirmedUnsolution(user: User, text: String): Unit = {
    unconfirmedUnsolutions += (user -> text)
  }
}
