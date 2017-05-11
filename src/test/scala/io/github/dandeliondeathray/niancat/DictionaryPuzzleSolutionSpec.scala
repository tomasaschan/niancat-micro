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

  "a DictionaryPuzzleSolution with a puzzle set" should "store a solution" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)
    solution.reset(Puzzle("TRIVASVAN")) // VANTRIVAS

    solution.solved(User("foo"), Word("VANTRIVAS"))

    solution.result shouldBe Some(SolutionResult(Map(Word("VANTRIVAS") -> Seq(User("foo")))))
  }

  it should "return solutions in the order in which they are found" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)
    solution.reset(Puzzle("TRIVASVAN")) // VANTRIVAS

    solution.solved(User("foo"), Word("VANTRIVAS"))
    solution.solved(User("bar"), Word("VANTRIVAS"))
    solution.solved(User("baz"), Word("VANTRIVAS"))

    solution.result shouldBe
      Some(SolutionResult(Map(Word("VANTRIVAS") -> Seq(User("foo"), User("bar"), User("baz")))))
  }

  it should "only list one solution if a user solves the same word several times" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)
    solution.reset(Puzzle("TRIVASVAN")) // VANTRIVAS

    solution.solved(User("foo"), Word("VANTRIVAS"))
    solution.solved(User("foo"), Word("VANTRIVAS"))

    solution.result shouldBe Some(SolutionResult(Map(Word("VANTRIVAS") -> Seq(User("foo")))))
  }

  "a DictionaryPuzzleSolution with several solutions" should "return all of them" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)
    solution.reset(Puzzle("DATORLESP")) // DATORSPEL, SPELDATOR, LEDARPOST, REPSOLDAT

    solution.solved(User("foo"), Word("DATORSPEL"))
    solution.solved(User("bar"), Word("DATORSPEL"))
    solution.solved(User("foo"), Word("SPELDATOR"))
    solution.solved(User("baz"), Word("LEDARPOST"))

    solution.result shouldBe Some(SolutionResult(
      Map(Word("DATORSPEL") -> Seq(User("foo"), User("bar")),
          Word("SPELDATOR") -> Seq(User("foo")),
          Word("LEDARPOST") -> Seq(User("baz")),
          Word("REPSOLDAT") -> Seq())))
  }
}
