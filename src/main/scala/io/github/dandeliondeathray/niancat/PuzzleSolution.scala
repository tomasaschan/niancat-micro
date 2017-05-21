package io.github.dandeliondeathray.niancat

case class SolutionResult(wordsAndSolvers: Map[Word, Seq[User]] = Map())

trait PuzzleSolution {
  def result: Option[SolutionResult]
  def reset(puzzle: Puzzle)
  def noOfSolutions(puzzle: Puzzle): Int
  def solved(user: User, word: Word)
  def solutionId(word: Word): Option[Int]
}

class DictionaryPuzzleSolution(val dictionary: Dictionary) extends PuzzleSolution {
  val solutions: Map[String, Seq[String]] = dictionary.toSeq map (_.letters) groupBy sortByCodePoints
  var solvedList: Seq[(Word, User)] = Seq()
  var puzzle: Option[Puzzle] = None

  override def result: Option[SolutionResult] = {
    if (puzzle == None) return None;
    val allSolutions: Seq[String] = puzzle map (p => solutions.getOrElse(sortByCodePoints(p.letters), Seq())) getOrElse(Seq())
    val allSolutionsMap: Map[Word, Seq[User]] = (allSolutions map (Word(_) -> Seq[User]())).toMap
    val resultMap = allSolutionsMap ++ (solvedList.distinct groupBy (_._1) mapValues (_.map(_._2)))
    Some(SolutionResult(resultMap))
  }

  override def reset(p: Puzzle): Unit = {
    puzzle = Some(p)
    solvedList = Seq()
  }

  override def noOfSolutions(puzzle: Puzzle): Int =
    solutions.getOrElse(sortByCodePoints(puzzle.letters), Seq()).size

  override def solved(user: User, word: Word): Unit = solvedList = solvedList :+ (word, user)

  override def solutionId(word: Word): Option[Int] = None

  private def sortByCodePoints(s: String): String = {
    val codePoints = s.codePoints() sorted() toArray
    val sb = new java.lang.StringBuilder()
    codePoints foreach (sb.appendCodePoint(_))
    sb.toString()
  }
}