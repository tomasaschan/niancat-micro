package io.github.dandeliondeathray.niancat

case class SolutionResult()

trait PuzzleSolution {
  def result: Option[SolutionResult]
  def reset(puzzle: Puzzle)
  def noOfSolutions(puzzle: Puzzle): Int
  def solved(user: User, word: Word)
}

class DictionaryPuzzleSolution(val dictionary: Dictionary) extends PuzzleSolution {
  val solutions: Map[String, Seq[String]] = dictionary.toSeq map (_.letters) groupBy sortByCodePoints

  override def result: Option[SolutionResult] = None

  override def reset(puzzle: Puzzle): Unit = {}

  override def noOfSolutions(puzzle: Puzzle): Int =
    solutions.getOrElse(sortByCodePoints(puzzle.letters), Seq()).size

  override def solved(user: User, word: Word): Unit = {}

  private def sortByCodePoints(s: String): String = {
    val codePoints = s.codePoints() sorted() toArray
    val sb = new java.lang.StringBuilder()
    codePoints foreach (sb.appendCodePoint(_))
    sb.toString()
  }
}