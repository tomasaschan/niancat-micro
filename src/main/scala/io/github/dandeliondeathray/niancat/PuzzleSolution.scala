package io.github.dandeliondeathray.niancat

case class SolutionResult(wordsAndSolvers: Map[Word, Seq[User]] = Map())

trait PuzzleSolution {
  def result: Option[SolutionResult]
  def reset(puzzle: Puzzle)
  def noOfSolutions(puzzle: Puzzle): Int
  def solved(user: User, word: Word)
}

class DictionaryPuzzleSolution(val dictionary: Dictionary) extends PuzzleSolution {
  val solutions: Map[String, Seq[String]] = dictionary.toSeq map (_.letters) groupBy sortByCodePoints
  var solvedList: Seq[(Word, User)] = Seq()
  var puzzle: Option[Puzzle] = None

  override def result: Option[SolutionResult] = {
    if (puzzle == None) return None;
    Some(SolutionResult(solvedList.distinct groupBy (_._1) mapValues (_.map(_._2))))
  }

  override def reset(p: Puzzle): Unit = puzzle = Some(p)

  override def noOfSolutions(puzzle: Puzzle): Int =
    solutions.getOrElse(sortByCodePoints(puzzle.letters), Seq()).size

  override def solved(user: User, word: Word): Unit = solvedList = solvedList :+ (word, user)

  private def sortByCodePoints(s: String): String = {
    val codePoints = s.codePoints() sorted() toArray
    val sb = new java.lang.StringBuilder()
    codePoints foreach (sb.appendCodePoint(_))
    sb.toString()
  }
}