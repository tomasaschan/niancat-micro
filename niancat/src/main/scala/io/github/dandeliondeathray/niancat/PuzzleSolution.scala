package io.github.dandeliondeathray.niancat

import org.scalactic._
import NormMethods._
import WordNormalizer._
import StringNormalizer._

case class SolutionResult(wordsAndSolvers: Map[Word, Seq[User]] = Map(), streaks: Map[User,Int] = Map())

trait PuzzleSolution {
  def result: Option[SolutionResult]
  def reset(puzzle: Puzzle)
  def noOfSolutions(puzzle: Puzzle): Int
  def solved(user: User, word: Word)
  def hasSolved(user: User, word: Word): Boolean
  def solutionId(word: Word): Option[Int]
  def streak(user: User): Int
}

class DictionaryPuzzleSolution(val dictionary: Dictionary) extends PuzzleSolution {
  val solutions: Map[String, Seq[String]] = dictionary.toSeq map (_.letters) groupBy sortByCodePoints
  var solvedList: Seq[(Word, User)] = Seq()
  var streaks: Map[User,Int] = Map()
  var puzzle: Option[Puzzle] = None

  override def result: Option[SolutionResult] = {
    if (puzzle == None) return None;
    val allSolutions: Seq[String] = puzzle map (p => solutions.getOrElse(sortByCodePoints(p.letters), Seq())) getOrElse(Seq())
    val allSolutionsMap: Map[Word, Seq[User]] = (allSolutions map (Word(_) -> Seq[User]())).toMap
    val resultMap = allSolutionsMap ++ (solvedList.distinct groupBy (_._1) mapValues (_.map(_._2)))
    Some(SolutionResult(resultMap, streaks))
  }

  override def reset(p: Puzzle): Unit = {
    puzzle = Some(p.norm)
    streaks = streaks filter { case (user,_) => solvedList.map(_._2) contains user }
    solvedList = Seq()
  }

  override def noOfSolutions(puzzle: Puzzle): Int =
    solutions.getOrElse(sortByCodePoints(puzzle.norm.letters), Seq()).size

  override def solved(user: User, word: Word): Unit = {
    streaks = streaks + (user -> ((streaks getOrElse (user, 0)) + (if (solvedList contains (word.norm, user)) 0 else 1)))
    solvedList = solvedList :+ (word.norm, user)
  }

  override def hasSolved(user: User, word: Word): Boolean = {
    solvedList contains (word.norm, user)
  }

  override def solutionId(word: Word): Option[Int] = {
    val allSolutionsForThisWord = solutions.getOrElse(sortByCodePoints(word.norm.letters), Seq())
    Some(allSolutionsForThisWord indexOf (word.norm.letters)) filter (_ != -1) map (_ + 1)
  }

  override def streak(user: User): Int = streaks getOrElse (user, 0)

  private def sortByCodePoints(s: String): String = {
    val codePoints = s.codePoints() sorted() toArray
    val sb = new java.lang.StringBuilder()
    codePoints foreach (sb.appendCodePoint(_))
    sb.toString()
  }
}
