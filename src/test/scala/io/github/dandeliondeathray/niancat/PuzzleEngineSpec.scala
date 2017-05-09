package io.github.dandeliondeathray.niancat

import org.scalatest._
import matchers._
import org.scalamock.scalatest._
import scala.reflect._

trait ResponseMatchers {
  class ContainsResponseMatcher(expectedResponse: Response) extends Matcher[Response] {
    def apply(response: Response) = {
      val hasOrIsExpectedResponse = response match {
        case CompositeResponse(responses: Vector[Response]) => responses contains expectedResponse
        case r: Response => r == expectedResponse
      }
      MatchResult(
        hasOrIsExpectedResponse,
        s"""Response $response did not contain expected response $expectedResponse""",
        s"""Response $response contained expected response $expectedResponse"""
      )
    }
  }

  class ContainsResponseTypeMatcher(responseType: Class[_]) extends Matcher[Response] {
    def apply(response: Response) = {
      val hasOrIsExpectedType = response match {
        case CompositeResponse(responses: Vector[Response]) => {
          responses exists (responseType == _.getClass)
        }
        case r: Response => responseType == r.getClass
      }

      MatchResult(
        hasOrIsExpectedType,
        s"""Response $response did not contain a response of type $responseType""",
        s"""Response $response did contain a response of type $responseType"""
      )
    }
  }

  def containResponse(expectedResponse: Response) = new ContainsResponseMatcher(expectedResponse)
  def containResponseOfType(responseType: Class[_]) = new ContainsResponseTypeMatcher(responseType)
}

/**
  * Created by Erik Edin on 2017-05-01.
  */
class PuzzleEngineSpec extends FlatSpec with Matchers with MockFactory with ResponseMatchers {
  val defaultPuzzle = Puzzle("VIVANSART")
  val defaultWord = Word("VANTRIVAS")

  def defaultSolutionResult = SolutionResult()

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

  def emptyPuzzleSolution: PuzzleSolution = {
    val puzzleSolution = stub[PuzzleSolution]
    (puzzleSolution.result _) when() returns(None) anyNumberOfTimes()
    (puzzleSolution.reset _) when(*) anyNumberOfTimes()
    (puzzleSolution.noOfSolutions _) when(*) returns(1) anyNumberOfTimes()

    puzzleSolution
  }

  /** Make a PuzzleEngine instance that accepts all words as correct. */
  def makeAcceptingPuzzleEngine(puzzle: Option[Puzzle] = None,
                                puzzleSolution: Option[PuzzleSolution] = None): PuzzleEngine = {
    val dictionary = acceptingDictionary
    new PuzzleEngine(dictionary, puzzleSolution getOrElse emptyPuzzleSolution, puzzle)
  }

  def makePuzzleEngine(dictionary: Dictionary,
                       puzzle: Option[Puzzle] = None,
                       puzzleSolution: Option[PuzzleSolution] = None): PuzzleEngine = {
    new PuzzleEngine(dictionary, puzzleSolution getOrElse emptyPuzzleSolution, puzzle)
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

  it should "not reply with a previous puzzle when a new one is set" in {
    val engine = makeAcceptingPuzzleEngine()

    val response = SetPuzzle(defaultPuzzle)(engine)

    response should not (containResponseOfType (classTag[YesterdaysPuzzle].runtimeClass))
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

    val mismatchingWord = Word("NOTRIGHTX")
    val response = CheckSolution(mismatchingWord, User("foo"))(engine)

    response shouldBe a [WordAndPuzzleMismatch]
  }

  it should "reply with incorrect length if the word is not nine letters long" in {
    val dictionary = rejectingDictionary
    val engine = makePuzzleEngine(dictionary, Some(defaultPuzzle))

    val wordIsWrongLength = Word("ABCDEF")
    val response = CheckSolution(wordIsWrongLength, User("foo"))(engine)

    response shouldBe IncorrectLength(wordIsWrongLength)
  }

  it should "notify the channel about yesterdays puzzle when a new one is set" in {
    val dictionary = acceptingDictionary

    // Return defaultWord as the solution to defaultPuzzle
    val puzzleSolution = stub[PuzzleSolution]
    (puzzleSolution.result _) when() returns(Some(defaultSolutionResult)) anyNumberOfTimes()
    (puzzleSolution.noOfSolutions _) when(*) returns(1) anyNumberOfTimes()

    val engine = makePuzzleEngine(dictionary, Some(defaultPuzzle), Some(puzzleSolution))

    val response = SetPuzzle(Puzzle("ABCDEFGHI"))(engine)

    response should containResponseOfType (classTag[YesterdaysPuzzle].runtimeClass)
  }

  it should "reset the puzzle solution when a new one is set" in {
    val dictionary = acceptingDictionary
    val newPuzzle = Puzzle("ABCDEFGHI")

    val puzzleSolution = mock[PuzzleSolution]
    (puzzleSolution.result _) expects() returning(Some(defaultSolutionResult)) anyNumberOfTimes()
    (puzzleSolution.reset _) expects (newPuzzle)
    (puzzleSolution.noOfSolutions _) expects(*) returning(1) anyNumberOfTimes()

    val engine = makePuzzleEngine(dictionary, Some(defaultPuzzle), Some(puzzleSolution))

    SetPuzzle(newPuzzle)(engine)
  }

  it should "respond that a puzzle is invalid if there are no solutions for it" in {
    val dictionary = acceptingDictionary
    val invalidPuzzle = Puzzle("ABCDEFGHI")

    val puzzleSolution = stub[PuzzleSolution]
    (puzzleSolution.noOfSolutions _) when(invalidPuzzle) returns(0) anyNumberOfTimes()
    // If we don't stub the result method, then this fails because of a NullPointerException,
    // which is not wrong, but misleading.
    (puzzleSolution.result _) when() returns(Some(SolutionResult())) anyNumberOfTimes()

    val engine = makePuzzleEngine(dictionary, Some(defaultPuzzle), Some(puzzleSolution))

    val response = SetPuzzle(invalidPuzzle)(engine)

    response shouldBe InvalidPuzzle(invalidPuzzle)
  }

  it should "not reset the puzzle if the new puzzle is invalid" in {
    val dictionary = acceptingDictionary
    val invalidPuzzle = Puzzle("ABCDEFGHI")

    val puzzleSolution = mock[PuzzleSolution]
    // PuzzleSolution.result should be a non-mutable method, so it doesn't matter if
    // it's being called or not.
    (puzzleSolution.result _) expects() returning(Some(SolutionResult())) anyNumberOfTimes()
    (puzzleSolution.reset _) expects(invalidPuzzle) never()
    (puzzleSolution.noOfSolutions _) expects(*) returning(0) anyNumberOfTimes()

    val engine = makePuzzleEngine(dictionary, Some(defaultPuzzle), Some(puzzleSolution))

    SetPuzzle(invalidPuzzle)(engine)
  }

}
