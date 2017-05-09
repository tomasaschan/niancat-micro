package io.github.dandeliondeathray.niancat

case class SolutionResult()

trait PuzzleSolution {
  def result: Option[SolutionResult]
  def reset(puzzle: Puzzle)
  def noOfSolutions(puzzle: Puzzle): Int
}