package io.github.dandeliondeathray.niancat

import org.scalatest._
import WritingSystemHelper._

class WritingSystemSpec extends FlatSpec with Matchers {

  "A nine letter word with an accent" should "be nine letters" in {
    Word("PIKÉTRÖJA").isNineLetters shouldBe true
  }

  "A word with ÅÄÖ" should "be nine letters" in {
    Word("ABCDEFÅÄÖ").isNineLetters shouldBe true
  }

  "A normal word" should "be nine letters" in {
    Word("ABCDEFGHI").isNineLetters shouldBe true
  }
}