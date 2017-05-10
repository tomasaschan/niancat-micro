package io.github.dandeliondeathray.niancat

import org.scalatest._
import org.scalamock.scalatest._

/**
  * Created by Erik Edin on 2017-05-09.
  */
class DictionaryPuzzleSolutionSpec extends FlatSpec with Matchers with MockFactory {
  val defaultWordSeq = Seq(
    Word("VANTRIVAS"), Word("PIKÉTRÖJA"), Word("ABCDEFGHI"),
    Word("DATORSPEL"), Word("SPELDATOR"), Word("LEDARPOST"), Word("REPSOLDAT")
  )

  def emptyDictionaryStub: Dictionary = {
    val dictionary = stub[Dictionary]
    (dictionary.toSeq _) when() returns(Seq[Word]()) anyNumberOfTimes()
    (dictionary.has _) when(*) returns(false) anyNumberOfTimes()

    dictionary
  }

  def defaultDictionaryStub: Dictionary = {
    val dictionary = stub[Dictionary]
    (dictionary.has _) when(*) returns(false) never()
    (dictionary.toSeq _) when() returns(defaultWordSeq)

    dictionary
  }

  "A default DictionaryPuzzleSolution" should "not have a result set" in {
    val solution = new DictionaryPuzzleSolution(emptyDictionaryStub)

    solution.result shouldBe None
  }

  it should "know the number of solutions to a given puzzle" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)

    solution.noOfSolutions(Puzzle("DATORLESP")) shouldBe 4
  }

  it should "say that an unknown word has zero solutions" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)

    solution.noOfSolutions(Puzzle("NOSUCHWRD")) shouldBe 0
  }
}
