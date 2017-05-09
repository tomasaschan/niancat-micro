package io.github.dandeliondeathray.niancat

import WritingSystemHelper._

/**
  * Created by Erik Edin on 2017-05-09.
  */
class NineLetterDictionary(strings: Seq[String]) extends Dictionary {
  val dictionary = strings map(Word(_)) filter (_.isNineLetters) toSet

  def has(word: Word): Boolean = dictionary.contains(word)
}

object NineLetterDictionary {
  def apply(strings: String*): NineLetterDictionary = {
    new NineLetterDictionary(strings)
  }
}