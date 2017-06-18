package io.github.dandeliondeathray.niancat

import org.scalactic.NormMethods._
import WordNormalizer._

object WritingSystemHelper {
  implicit class WritingSystem(word: Word) {
    def isNineLetters: Boolean = word.norm.letters.codePoints().count == 9
  }
}
