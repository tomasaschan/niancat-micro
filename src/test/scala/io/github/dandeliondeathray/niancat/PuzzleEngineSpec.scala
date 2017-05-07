package io.github.dandeliondeathray.niancat

import org.scalatest._
import matchers._
import org.scalamock.scalatest._

trait ResponseMatchers {
  class ContainsResponseMatcher(expectedResponse: Response) extends Matcher[Response] {
    def apply(response: Response) = {
      val hasOrIsExpectedResponse = response match {
        case CompositeResponse(responses: Vector[Response]) => responses contains expectedResponse
        case r: Response => r == expectedResponse
      }
      MatchResult(
        hasOrIsExpectedResponse,
        s"""Response $response did not contain expected reply $expectedResponse""",
        s"""Response $response contained expected reply $expectedResponse"""
      )
    }
  }

  def containResponse(expectedResponse: Response) = new ContainsResponseMatcher(expectedResponse)
}

/**
  * Created by Erik Edin on 2017-05-01.
  */
class PuzzleEngineSpec extends FlatSpec with Matchers with MockFactory with ResponseMatchers {
  val defaultPuzzle = Puzzle("VIVANSART")
  val defaultWord = Word("VANTRIVAS")

  def acceptingDictionary: Dictionary = {
    val dictionary = stub[Dictionary]
    (dictionary.has _) when(*) returns(true) anyNumberOfTimes()
    dictionary
  }

  def rejectingDictionary: Dictionary = {
    val dictionary = stub[Dictionary]
    (dictionary.has _) when(*) returns(false) anyNumberOfTimes()
    dictionary
  }

  /** Make a PuzzleEngine instance that accepts all words as correct. */
  def makeAcceptingPuzzleEngine(puzzle: Option[Puzzle] = None): PuzzleEngine = {
    val dictionary = acceptingDictionary
    new PuzzleEngine(dictionary, puzzle)
  }

  def makePuzzleEngine(dictionary: Dictionary,
                       puzzle: Option[Puzzle] = None): PuzzleEngine = {
    new PuzzleEngine(dictionary, puzzle)
  }

  "An engine with no puzzle set" should "reply that no puzzle is set, when asked for the puzzle" in {
    val engine = makeAcceptingPuzzleEngine()
    val response = Get()(engine)
    response shouldBe NoPuzzleSet()
  }

  it should "notify that a new puzzle is set" in {
    val engine = makeAcceptingPuzzleEngine()
    val response = SetPuzzle(defaultPuzzle)(engine)

    response should containResponse (NewPuzzle(defaultPuzzle))
  }

  it should "store the new puzzle" in {
    val engine = makeAcceptingPuzzleEngine()
    SetPuzzle(defaultPuzzle)(engine)

    engine.puzzle shouldBe Some(defaultPuzzle)
  }

  it should "reply that no puzzle is set, when a user checks a solution" in {
    val engine = makeAcceptingPuzzleEngine()

    val response = CheckSolution(defaultWord, User("foo"))(engine)

    response shouldBe NoPuzzleSet()
  }

  "An engine with a puzzle set" should "reply with the puzzle, when asked for the puzzle" in {
    val engine = makeAcceptingPuzzleEngine(Some(defaultPuzzle))
    val response = Get()(engine)
    response shouldBe GetReply(defaultPuzzle)
  }

  it should "reply that a word is not in the dictionary, when a user checks a word" in {
    val dictionary = rejectingDictionary
    val engine = makePuzzleEngine(dictionary, Some(defaultPuzzle))

    val response = CheckSolution(defaultWord, User("foo"))(engine)

    response shouldBe NotInTheDictionary(defaultWord)
  }

  it should "reply that a word is correct, if the word is in the dictionary" in {
    val dictionary = acceptingDictionary
    val engine = makePuzzleEngine(dictionary, Some(defaultPuzzle))

    val response = CheckSolution(defaultWord, User("foo"))(engine)

    response should containResponse (CorrectSolution(defaultWord))
  }

  it should "reply with a mismatch if the word does not match the puzzle" in {
    val dictionary = rejectingDictionary
    val engine = makePuzzleEngine(dictionary, Some(defaultPuzzle))

    val mismatchingWord = Word("NOTRIGHT")
    val response = CheckSolution(mismatchingWord, User("foo"))(engine)

    response shouldBe a [WordAndPuzzleMismatch]
  }
}
