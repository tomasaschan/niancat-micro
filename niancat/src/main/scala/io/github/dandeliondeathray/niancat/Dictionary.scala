package io.github.dandeliondeathray.niancat

trait Dictionary {
  def has(word: Word): Boolean
  def toSeq: Seq[Word]
}
