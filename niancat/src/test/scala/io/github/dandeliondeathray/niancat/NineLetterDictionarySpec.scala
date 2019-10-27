package io.github.dandeliondeathray.niancat

import org.scalatest._

/**
  * Created by Erik Edin on 2017-05-09.
  */
class NineLetterDictionarySpec extends FlatSpec with Matchers {
  def dictionary = NineLetterDictionary(
    "A",
    "AB",
    "ABC",
    "ABCDEFGHI",
    "BCDEFGHIJ",
    "PIKÉTRÖJA",
    "VANTRIVAS",
    "ABCDEFGHIJKL",
    "BCDEFGHIJKLM"
  )

  "the dictionary" should "contain ABCDEFGHI" in {
    dictionary.has(Word("ABCDEFGHI")) shouldBe true
  }

  it should "not contain missing word DATORSPEL" in {
    dictionary.has(Word("DATORSPEL")) shouldBe false
  }

  it should "contain special word PIKÉTRÖJA" in {
    dictionary.has(Word("PIKÉTRÖJA")) shouldBe true
  }

  it should "not contain short word ABC" in {
    dictionary.has(Word("ABC")) shouldBe false
  }

  it should "not contain long word ABCDEFGHIJKL" in {
    dictionary.has(Word("ABCDEFGHIJKL")) shouldBe false
  }

  it should "normalize words in constructor and lookup" in {
    dictionary.has(Word("piketröja")) shouldBe true
  }
}
