package io.github.dandeliondeathray.niancat

import WritingSystemHelper._

/**
  * Created by Erik Edin on 2017-05-09.
  */
class NineLetterDictionary(strings: Seq[String]) extends Dictionary {
  val dictionary = strings map(Word(_)) filter (_.isNineLetters) toSet

  override def has(word: Word): Boolean = dictionary.contains(word)
  override def toSeq: Seq[Word] = dictionary.toSeq
}

object NineLetterDictionary {
  def apply(strings: String*): NineLetterDictionary = {
    new NineLetterDictionary(strings)
  }
}