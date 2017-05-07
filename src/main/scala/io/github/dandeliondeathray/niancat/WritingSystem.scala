package io.github.dandeliondeathray.niancat

object WritingSystemHelper {
  implicit class WritingSystem(word: Word) {
    def isNineLetters: Boolean = word.letters.codePoints().count == 9
  }
}
