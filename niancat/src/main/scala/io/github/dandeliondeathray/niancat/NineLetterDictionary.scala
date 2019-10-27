package io.github.dandeliondeathray.niancat

import WritingSystemHelper._
import org.scalactic._
import NormMethods._
import WordNormalizer._

/**
  * Created by Erik Edin on 2017-05-09.
  */
class NineLetterDictionary(strings: Seq[String]) extends Dictionary {
  val dictionary = strings map (Word(_).norm) filter (_.isNineLetters) toSet

  override def has(word: Word): Boolean = dictionary.contains(word.norm)
  override def toSeq: Seq[Word] = dictionary.toSeq
}

object NineLetterDictionary {
  def apply(strings: String*): NineLetterDictionary = {
    new NineLetterDictionary(strings)
  }
}
