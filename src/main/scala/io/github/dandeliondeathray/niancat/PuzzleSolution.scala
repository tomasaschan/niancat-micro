package io.github.dandeliondeathray.niancat

trait PuzzleSolution {
  def solution: Option[Word]
  def reset(puzzle: Puzzle)
}