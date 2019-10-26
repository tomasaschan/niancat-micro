package io.github.dandeliondeathray.niancat

import org.scalatest._
import org.scalamock.scalatest._

import org.scalactic._
import NormMethods._
import StringNormalizer._
import WordNormalizer._

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
    (dictionary.toSeq _) when() returns(defaultWordSeq map (_.norm))

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
    solution.reset(Puzzle("TRIVASVAN"), true) // VANTRIVAS

    solution.solved(User("foo"), Word("VANTRIVAS"), true)

    solution.result shouldBe Some(SolutionResult(Map(Word("VANTRIVAS") -> Seq(User("foo"))), Map(User("foo") -> 1)))
  }

  it should "not increase streak on weekends" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)
    solution.reset(Puzzle("TRIVASVAN"), true) // VANTRIVAS

    solution.solved(User("foo"), Word("VANTRIVAS"), false)

    solution.result shouldBe Some(SolutionResult(Map(Word("VANTRIVAS") -> Seq(User("foo"))), Map()))
  }

  it should "return solutions in the order in which they are found" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)
    solution.reset(Puzzle("TRIVASVAN"), true) // VANTRIVAS

    solution.solved(User("foo"), Word("VANTRIVAS"), true)
    solution.solved(User("bar"), Word("VANTRIVAS"), true)
    solution.solved(User("baz"), Word("VANTRIVAS"), true)

    solution.result shouldBe
      Some(SolutionResult(Map(Word("VANTRIVAS") -> Seq(User("foo"), User("bar"), User("baz"))), Map(User("foo") -> 1, User("bar") -> 1, User("baz") -> 1)))
  }

  it should "only list one solution if a user solves the same word several times" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)
    solution.reset(Puzzle("TRIVASVAN"), true) // VANTRIVAS

    solution.solved(User("foo"), Word("VANTRIVAS"), true)
    solution.solved(User("foo"), Word("VANTRIVAS"), true)

    solution.result shouldBe Some(SolutionResult(Map(Word("VANTRIVAS") -> Seq(User("foo"))), Map(User("foo") -> 1)))
  }

  it should "forget solutions when a new puzzle is set" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)
    solution.reset(Puzzle("GURKPUSSA"), true)
    solution.solved(User("foo"), Word("PUSSGURKA"), true)
    solution.solved(User("baz"), Word("PUSSGURKA"), true)

    solution.reset(Puzzle("DATORLESP"), true)

    solution.solved(User("foo"), Word("DATORSPEL"), true)
    solution.solved(User("bar"), Word("DATORSPEL"), true)

    solution.result shouldBe Some(
      SolutionResult(
        Map(
          Word("DATORSPEL") -> Seq(User("foo"), User("bar")),
          Word("SPELDATOR") -> Seq(),
          Word("REPSOLDAT") -> Seq(),
          Word("LEDARPOST") -> Seq()
        ),
        Map(User("foo") -> 2, User("bar") -> 1, User("baz") -> 1)
      )
    )

    solution.reset(Puzzle("VANTRIVAS"), true)

    solution.result shouldBe Some(
      SolutionResult(
        Map(Word("VANTRIVAS") -> Seq()),
        Map(User("foo") -> 2, User("bar") -> 1)
      )
    )
  }

  it should "not forget streaks when new puzzle is set on weekends" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)
    solution.reset(Puzzle("GURKPUSSA"), true)
    solution.solved(User("foo"), Word("PUSSGURKA"), true)
    solution.solved(User("baz"), Word("PUSSGURKA"), true)

    solution.reset(Puzzle("DATORLESP"), true)

    solution.solved(User("foo"), Word("DATORSPEL"), true)
    solution.solved(User("bar"), Word("DATORSPEL"), true)

    solution.result shouldBe Some(
      SolutionResult(
        Map(
          Word("DATORSPEL") -> Seq(User("foo"), User("bar")),
          Word("SPELDATOR") -> Seq(),
          Word("REPSOLDAT") -> Seq(),
          Word("LEDARPOST") -> Seq()
        ),
        Map(User("foo") -> 2, User("bar") -> 1, User("baz") -> 1)
      )
    )

    solution.reset(Puzzle("VANTRIVAS"), false)

    solution.result shouldBe Some(
      SolutionResult(
        Map(Word("VANTRIVAS") -> Seq()),
        Map(User("foo") -> 2, User("bar") -> 1, User("baz") -> 1)
      )
    )
  }

  it should "normalize the puzzle on reset" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)
    val puzzle = Puzzle("piketröja")

    solution.reset(puzzle, true)

    solution.puzzle shouldBe Some(puzzle.norm)
  }

  it should "normalize words that users solve" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)

    solution.reset(Puzzle("PIKÉTRÖJA"), true)
    val word = Word("pikétröja")
    solution.solved(User("foo"), word, true)

    solution.result shouldBe Some(SolutionResult(Map(word.norm -> Seq(User("foo"))), Map(User("foo") -> 1)))
  }

  it should "find a single solution even for a non-normalized puzzle" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)

    solution.reset(Puzzle("PIKÉTRÖJA"), true)

    solution.noOfSolutions(Puzzle("piketröja")) shouldBe 1
  }

  "a DictionaryPuzzleSolution with several solutions" should "return all of them" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)
    solution.reset(Puzzle("DATORLESP"), true) // DATORSPEL, SPELDATOR, LEDARPOST, REPSOLDAT

    solution.solved(User("foo"), Word("DATORSPEL"), true)
    solution.solved(User("bar"), Word("DATORSPEL"), true)
    solution.solved(User("foo"), Word("SPELDATOR"), true)
    solution.solved(User("baz"), Word("LEDARPOST"), true)

    solution.result shouldBe Some(SolutionResult(
      Map(Word("DATORSPEL") -> Seq(User("foo"), User("bar")),
          Word("SPELDATOR") -> Seq(User("foo")),
          Word("LEDARPOST") -> Seq(User("baz")),
          Word("REPSOLDAT") -> Seq()),
      Map(User("foo") -> 2,
          User("bar") -> 1,
          User("baz") -> 1)))
  }

  it should "return the same solution id for a given word every time" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)
    solution.reset(Puzzle("DATORLESP"), true) // DATORSPEL, SPELDATOR, LEDARPOST, REPSOLDAT
    val word = Word("DATORSPEL")

    val solutionIds = 1 until 10 map (_ => solution.solutionId(word))
    val first = solutionIds.head

    assert (solutionIds forall (_ == first))
  }

  it should "return solution ids 1-4 in some order, if it has four solutions" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)
    solution.reset(Puzzle("DATORLESP"), true) // DATORSPEL, SPELDATOR, LEDARPOST, REPSOLDAT

    val words = List(Word("DATORSPEL"), Word("SPELDATOR"), Word("LEDARPOST"), Word("REPSOLDAT"))
    val solutionIds = words map (solution.solutionId(_)) flatten

    assert (Set(solutionIds: _*) == Set(1, 2, 3, 4))
  }

  it should "return the solution id even if there is only one solution" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)
    solution.reset(Puzzle("VANTRIVSA"), true) // VANTRIVAS

    solution.solutionId(Word("VANTRIVAS")) shouldBe Some(1)
  }

  it should "normalize words to find the solution id" in {
    val solution = new DictionaryPuzzleSolution(defaultDictionaryStub)
    solution.reset(Puzzle("PIKÉTRÖAJ"), true) // PIKÉTRÖJA

    solution.solutionId(Word("piketröja")) shouldBe Some(1)
  }
}
