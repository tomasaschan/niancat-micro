package io.github.dandeliondeathray.niancat

import org.scalatest._

class NormalizationSpec extends FlatSpec with Matchers {
  "a Word and a Puzzle" should "match when they are the same" in {
    Word("ABCDEFGHI") matches Puzzle("ABCDEFGHI") shouldBe true
  }

  it should "mismatch when they have different characters" in {
    Word("ABCDEFGHI") matches Puzzle("DEFGHIJKL") shouldBe false
  }

  it should "match if they only differ by case" in {
    Word("abcdefghi") matches Puzzle("ABCDEFGHI") shouldBe true
  }

  it should "match if they differ only in accents" in {
    Word("PIKÉTRÖJA") matches Puzzle("PIKETRÖJA") shouldBe true
  }

  it should "mismatch if they differ in umlauts" in {
    Word("PIKETROJA") matches Puzzle("PIKETRÖJA") shouldBe false
  }
}