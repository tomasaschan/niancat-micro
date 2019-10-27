package io.github.dandeliondeathray.niancat

import org.scalatest._
import matchers._
import org.scalamock.scalatest._
import scala.reflect._

import org.scalactic.NormMethods._
import StringNormalizer._
import PuzzleNormalizer._

trait ResponseMatchers {
  class ContainsResponseMatcher(expectedResponse: Response) extends Matcher[Response] {
    def apply(response: Response) = {
      val hasOrIsExpectedResponse = response match {
        case CompositeResponse(responses: Vector[Response]) => responses contains expectedResponse
        case r: Response                                    => r == expectedResponse
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
  val shuffledPuzzle = Puzzle("ARTSVIVAN")
  val defaultWord = Word("VANTRIVAS")

  def defaultSolutionResult = SolutionResult()

  def acceptingDictionary: Dictionary = {
    val dictionary = stub[Dictionary]
    (dictionary.has _) when (*) returns (true) anyNumberOfTimes ()
    (dictionary.solutions _) when (*) returns (Seq(Word("VANTRIVAS"))) anyNumberOfTimes ()
    dictionary
  }

  def rejectingDictionary: Dictionary = {
    val dictionary = stub[Dictionary]
    (dictionary.has _) when (*) returns (false) anyNumberOfTimes ()
    dictionary
  }

  def acceptingMultipleSolutionsDictionary: Dictionary = {
    val dictionary = stub[Dictionary]
    (dictionary.has _) when (*) returns (true) anyNumberOfTimes ()
    (dictionary.solutions _) when (*) returns (Seq(
      Word("SPELDATOR"),
      Word("DATORSPEL"),
      Word("REPSOLDAT"),
      Word("LEDARPOST")
    )) anyNumberOfTimes ()
    (dictionary.solutionId _) when (*, *) returns (Some(1)) anyNumberOfTimes ()
    dictionary
  }

  def emptyState: State = {
    val puzzleSolution = stub[State]
    (puzzleSolution.result _) when (*) returns (None) anyNumberOfTimes ()
    (puzzleSolution.reset _) when (*, *) anyNumberOfTimes ()
    (puzzleSolution.solved _) when (*, *, *) anyNumberOfTimes ()

    puzzleSolution
  }

  def engineWithPuzzle(puzzle: Puzzle, dictionary: Dictionary = acceptingDictionary): PuzzleEngine = {
    val state = emptyState
    state.reset(puzzle, true)
    new PuzzleEngine(state, dictionary)
  }

  "An engine with no puzzle set" should "reply that no puzzle is set, when asked for the puzzle" in {
    val engine = new PuzzleEngine(emptyState, acceptingDictionary)
    val response = Get()(engine)
    response shouldBe NoPuzzleSet()
  }

  it should "notify that a new puzzle is set" in {
    val engine = new PuzzleEngine(emptyState, acceptingDictionary)
    val response = SetPuzzle(defaultPuzzle, true)(engine)

    response should containResponse(NewPuzzle(defaultPuzzle))
  }

  it should "normalize the puzzle when notifying that a new one is set" in {
    val engine = new PuzzleEngine(emptyState, acceptingDictionary)
    val puzzle = Puzzle("pikétröja")
    val response = SetPuzzle(puzzle, true)(engine)

    response should containResponse(NewPuzzle(puzzle.norm))
  }

  it should "reply that no puzzle is set, when a user checks a solution" in {
    val engine = new PuzzleEngine(emptyState, acceptingDictionary)

    val response = CheckSolution(defaultWord, User("foo"), true)(engine)

    response shouldBe NoPuzzleSet()
  }

  it should "not reply with a previous puzzle when a new one is set" in {
    val engine = new PuzzleEngine(emptyState, acceptingDictionary)

    val response = SetPuzzle(defaultPuzzle, true)(engine)

    response should not(containResponseOfType(classTag[YesterdaysPuzzle].runtimeClass))
  }

  it should "reply that no puzzle is set if a user tries to store an unsolution" in {
    val engine = new PuzzleEngine(emptyState, acceptingDictionary)

    val response = AddUnsolution("Some unsolution", User("foo"))(engine)

    response shouldBe NoPuzzleSet()
  }

  "An engine with a puzzle set" should "reply with the puzzle, when asked for the puzzle" in {
    val engine = engineWithPuzzle(defaultPuzzle)
    val response = Get()(engine)
    response shouldBe GetReply(defaultPuzzle)
  }

  it should "reply that a word is not in the dictionary, when a user checks a word" in {
    val engine = engineWithPuzzle(defaultPuzzle, rejectingDictionary)

    val response = CheckSolution(defaultWord, User("foo"), true)(engine)

    response shouldBe NotInTheDictionary(defaultWord)
  }

  it should "reply that a word is correct, if the word is in the dictionary" in {
    val engine = engineWithPuzzle(defaultPuzzle)

    val response = CheckSolution(defaultWord, User("foo"), true)(engine)

    response should containResponse(CorrectSolution(defaultWord))
  }

  it should "reply with a mismatch if the word does not match the puzzle" in {
    val engine = engineWithPuzzle(defaultPuzzle, rejectingDictionary)

    val mismatchingWord = Word("NOTRIGHTX")
    val response = CheckSolution(mismatchingWord, User("foo"), true)(engine)

    response shouldBe a[WordAndPuzzleMismatch]
  }

  it should "show the users the mismatching letters" in {
    val puzzle = Puzzle("ABCDEFGHI")
    val mismatchingWord = Word("DEFGHIJKL")
    val engine = engineWithPuzzle(puzzle)

    val response = CheckSolution(mismatchingWord, User("foo"), true)(engine)

    response should have('tooMany ("JKL"), 'tooFew ("ABC"))
  }

  it should "repeat mismatching letters the correct number of times" in {
    val puzzle = Puzzle("ABCDEFGHI")
    val mismatchingWord = Word("AAADEFGHI")
    val engine = engineWithPuzzle(puzzle)

    val response = CheckSolution(mismatchingWord, User("foo"), true)(engine)

    response should have('tooMany ("AA"), 'tooFew ("BC"))
  }

  it should "reply with mismatch if the word is not nine letters long" in {
    val puzzle = Puzzle("ABCDEFGHI")
    val engine = engineWithPuzzle(puzzle, rejectingDictionary)

    val wordIsWrongLength = Word("ABCDEF")
    val response = CheckSolution(wordIsWrongLength, User("foo"), true)(engine)

    response shouldBe IncorrectLength(wordIsWrongLength, None, Some("GHI"))
  }

  it should "notify the channel about yesterdays puzzle when a new one is set" in {
    // Return defaultWord as the solution to defaultPuzzle
    val state = stub[State]
    (state.result _) when (*) returns (Some(defaultSolutionResult)) anyNumberOfTimes ()

    val engine = new PuzzleEngine(state, acceptingDictionary)

    val response = SetPuzzle(Puzzle("ABCDEFGHI"), true)(engine)

    response should containResponseOfType(classTag[YesterdaysPuzzle].runtimeClass)
  }

  it should "reset the puzzle solution when a new one is set" in {
    val newPuzzle = Puzzle("ABCDEFGHI")

    val state = mock[State]
    (state.result _) expects (*) returning (Some(defaultSolutionResult)) anyNumberOfTimes ()
    (state.reset _) expects (newPuzzle, true)

    val engine = new PuzzleEngine(state, acceptingDictionary)

    SetPuzzle(newPuzzle, true)(engine)
  }

  it should "respond that a puzzle is invalid if there are no solutions for it" in {
    val invalidPuzzle = Puzzle("ABCDEFGHI")

    val engine = new PuzzleEngine(emptyState, rejectingDictionary)

    val response = SetPuzzle(invalidPuzzle, true)(engine)

    response shouldBe InvalidPuzzle(invalidPuzzle)
  }

  it should "not reset the puzzle if the new puzzle is invalid" in {
    val invalidPuzzle = Puzzle("ABCDEFGHI")

    val state = mock[State]
    (state.reset _) expects (invalidPuzzle, true) never ()

    val engine = new PuzzleEngine(state, rejectingDictionary)

    SetPuzzle(invalidPuzzle, true)(engine)
  }

  it should "let the users know if a puzzle has more than one solution" in {
    val dictionary = acceptingMultipleSolutionsDictionary
    val newPuzzle = Puzzle("ABCDEFGHI")

    val state = stub[State]
    (state.reset _) when (*, *) anyNumberOfTimes ()

    val engine = new PuzzleEngine(state, dictionary)

    val response = SetPuzzle(newPuzzle, true)(engine)

    response should containResponse(MultipleSolutions(4))
  }

  it should "not mention multiple solutions if there is only one" in {
    val newPuzzle = Puzzle("ABCDEFGHI")

    val engine = engineWithPuzzle(Puzzle("VANTRIVAS"), acceptingDictionary)

    val response = SetPuzzle(newPuzzle, true)(engine)

    response should not(containResponseOfType(classTag[MultipleSolutions].runtimeClass))
  }

  it should "store a solution" in {
    val state = mock[State]
    (state.solved _) expects (User("foo"), defaultWord, true)
    (state.streak _) expects (User("foo")) returns (1) anyNumberOfTimes ()
    (state.hasSolved _) expects (User("foo"), defaultWord) returning (false) anyNumberOfTimes ()

    val engine = new PuzzleEngine(state, acceptingDictionary)

    val response = CheckSolution(defaultWord, User("foo"), true)(engine)
  }

  it should "not store an invalid solution" in {
    val state = mock[State]
    (state.solved _) expects (User("foo"), defaultWord, true) never ()

    val engine = new PuzzleEngine(state, rejectingDictionary)

    val response = CheckSolution(defaultWord, User("foo"), true)(engine)
  }

  it should "not reset the puzzle if the new puzzle is the same as the old" in {
    val state = mock[State]
    (state.reset _) expects (*, *) never ()

    val engine = new PuzzleEngine(state, acceptingDictionary)

    SetPuzzle(defaultPuzzle, true)(engine)
  }

  it should "respond with SamePuzzle if the new puzzle is an anagram of the old" in {
    val state = stub[State]
    (state.reset _) when (*, *) anyNumberOfTimes ()
    (state.result _) when (*) returns (Some(SolutionResult())) anyNumberOfTimes ()

    val engine = new PuzzleEngine(state, acceptingDictionary)

    val response = SetPuzzle(shuffledPuzzle, true)(engine)

    response shouldBe SamePuzzle(shuffledPuzzle)
  }

  it should "respond with SamePuzzle if the new puzzle is the same as the old" in {
    val state = stub[State]
    (state.reset _) when (*, *) anyNumberOfTimes ()
    (state.result _) when (*) returns (Some(SolutionResult())) anyNumberOfTimes ()

    val engine = new PuzzleEngine(state, acceptingDictionary)

    val response = SetPuzzle(defaultPuzzle, true)(engine)

    response shouldBe SamePuzzle(defaultPuzzle)
  }

  it should "normalize the puzzle when seeing if it's the same one" in {
    val puzzle = Puzzle("PIKETRÖJA")

    val state = stub[State]
    (state.reset _) when (*, *) anyNumberOfTimes ()
    (state.result _) when (*) returns (Some(SolutionResult())) anyNumberOfTimes ()

    val engine = new PuzzleEngine(state, acceptingDictionary)

    val response = SetPuzzle(Puzzle("pikétröja"), true)(engine)

    response shouldBe SamePuzzle(puzzle)
  }

  it should "notify the main channel if a user solves the puzzle" in {
    val engine = engineWithPuzzle(defaultPuzzle)

    val response = CheckSolution(defaultWord, User("foo"), true)(engine)

    response should containResponse(SolutionNotification(User("foo"), 1, None))
  }

  it should "not notify the main channel if a user solves the puzzle again" in {
    val state = stub[State]
    (state.result _) when (*) returns (None) anyNumberOfTimes ()
    (state.reset _) when (*, *) anyNumberOfTimes ()
    (state.solved _) when (*, *, *) anyNumberOfTimes ()
    inSequence {
      (state.hasSolved _) when (*, *) returns (false) once ()
      (state.hasSolved _) when (*, *) returns (true) anyNumberOfTimes ()
    } anyNumberOfTimes ()

    val engine = new PuzzleEngine(state, acceptingDictionary)

    val firstResponse = CheckSolution(defaultWord, User("foo"), true)(engine)

    firstResponse should containResponse(CorrectSolution(defaultWord))
    firstResponse should containResponse(SolutionNotification(User("foo"), 1, None))

    val secondResponse = CheckSolution(defaultWord, User("foo"), true)(engine)

    secondResponse shouldBe CorrectSolution(defaultWord)
  }

  it should "not include solution id in the solution notification is there is only one solution" in {
    val engine = engineWithPuzzle(defaultPuzzle)

    val response = CheckSolution(defaultWord, User("foo"), true)(engine)

    response should containResponse(SolutionNotification(User("foo"), 1, None))
  }

  it should "include the solution if there are multiple solutions" in {
    val solutionId = 3
    val dictionary = stub[Dictionary]
    (dictionary.solutions _) when (*) returns (Seq(
      Word("DATORSPEL"),
      Word("SPELDATOR"),
      Word("LEDARPOST"),
      Word("REPSOLDAT")
    )) anyNumberOfTimes ()
    (dictionary.solutionId _) when (*, *) returns (Some(solutionId)) anyNumberOfTimes ()

    val state = stub[State]
    (state.result _) when (*) returns (Some(defaultSolutionResult)) anyNumberOfTimes ()
    (state.reset _) when (*, *) anyNumberOfTimes ()

    val engine = engineWithPuzzle(defaultPuzzle, dictionary)

    val response = CheckSolution(defaultWord, User("foo"), true)(engine)

    response should containResponse(SolutionNotification(User("foo"), 1, Some(solutionId)))
  }

  it should "not send a response when adding an unsolution" in {
    val engine = engineWithPuzzle(defaultPuzzle)

    val response = AddUnsolution(s"Some ${defaultPuzzle.letters}", User("foo"))(engine)

    response shouldBe UnsolutionAdded()
  }

  it should "store an unsolution for later listing" in {
    val engine = engineWithPuzzle(defaultPuzzle)
    val unsolutionText = s"Unsolution ${defaultPuzzle.letters}"

    AddUnsolution(unsolutionText, User("foo"))(engine)
    val response = ListUnsolutions(User("foo"))(engine)

    response shouldBe Unsolutions(List(unsolutionText))
  }

  it should "separate unsolutions based on users" in {
    val engine = engineWithPuzzle(defaultPuzzle)

    AddUnsolution(s"Some ${defaultPuzzle.letters}", User("foo"))(engine)
    val response = ListUnsolutions(User("otheruser"))(engine)

    response shouldBe NoUnsolutions()
  }

  it should "be able to store multiple unsolutions, in order" in {
    val engine = engineWithPuzzle(defaultPuzzle)
    val unsolutionText1 = s"Some ${defaultPuzzle.letters}"
    val unsolutionText2 = s"Other ${defaultPuzzle.letters}"

    AddUnsolution(unsolutionText1, User("foo"))(engine)
    AddUnsolution(unsolutionText2, User("foo"))(engine)
    val response = ListUnsolutions(User("foo"))(engine)

    response should have('texts (List(unsolutionText1, unsolutionText2)))
  }

  it should "clear all unsolutions when a new puzzle is set" in {
    val engine = engineWithPuzzle(defaultPuzzle)

    AddUnsolution(s"Some ${defaultPuzzle.letters}", User("foo"))(engine)
    SetPuzzle(Puzzle("DATORSPEL"), true)(engine)

    val response = ListUnsolutions(User("foo"))(engine)

    response shouldBe NoUnsolutions()
  }

  it should "reply with all unsolutions if a new puzzle is set" in {
    val engine = engineWithPuzzle(defaultPuzzle)
    val textFoo1 = s"Some ${defaultPuzzle.letters}"
    val textFoo2 = s"Some other ${defaultPuzzle.letters}"
    val textBar1 = s"Some ${defaultPuzzle.letters} for another user"

    AddUnsolution(textFoo1, User("foo"))(engine)
    AddUnsolution(textFoo2, User("foo"))(engine)
    AddUnsolution(textBar1, User("bar"))(engine)

    val response = SetPuzzle(Puzzle("DATORSPEL"), true)(engine)

    response should containResponse(
      AllUnsolutions(Map(User("foo") -> List(textFoo1, textFoo2), User("bar") -> List(textBar1)))
    )
  }

  it should "not mention unsolutions when a new puzzle is set, if there are no unsolutions" in {
    val engine = engineWithPuzzle(defaultPuzzle)

    val response = SetPuzzle(Puzzle("DATORSPEL"), true)(engine)

    response shouldNot containResponseOfType(classTag[AllUnsolutions].runtimeClass)
  }

  it should "reply with unconfirmed unsolutions if no word matches the puzzle" in {
    val engine = engineWithPuzzle(defaultPuzzle)
    val unsolutionText = s"No word matches the puzzle"

    val response = AddUnsolution(unsolutionText, User("foo"))(engine)

    response shouldBe UnsolutionNeedsConfirmation(defaultPuzzle)
  }

  it should "save an unconfirmed unsolutions if it's saved a second time" in {
    val engine = engineWithPuzzle(defaultPuzzle)
    val unsolutionText = s"No word matches the puzzle"

    AddUnsolution(unsolutionText, User("foo"))(engine)
    val response = AddUnsolution(unsolutionText, User("foo"))(engine)

    response shouldBe UnsolutionAdded()
  }

  it should "forget the unconfirmed unsolution if another unsolution comes before a confirmation" in {
    val engine = engineWithPuzzle(defaultPuzzle)
    val unsolutionText = s"No word matches the puzzle"

    AddUnsolution(unsolutionText, User("foo"))(engine)
    AddUnsolution(s"${defaultPuzzle.letters}", User("foo"))(engine)
    val response = AddUnsolution(unsolutionText, User("foo"))(engine)

    response shouldBe UnsolutionNeedsConfirmation(defaultPuzzle)
  }

  it should "save an unconfirmed solution when we receive an empty unsolution" in {
    val engine = engineWithPuzzle(defaultPuzzle)
    val unsolutionText = "No word matches the puzzle"

    AddUnsolution(unsolutionText, User("foo"))(engine)
    val response = AddUnsolution("", User("foo"))(engine)

    response shouldBe UnsolutionAdded()
  }

  it should "state that there is no unsolution to confirm for an emty unsolution command" in {
    val engine = engineWithPuzzle(defaultPuzzle)

    val response = AddUnsolution("", User("foo"))(engine)

    response shouldBe NoUnsolutionToConfirm()
  }

  it should "save the unconfirmed unsolution in case you use the empty add unsolution command" in {
    val engine = engineWithPuzzle(defaultPuzzle)
    val unsolutionText = "No word matches the puzzle"

    AddUnsolution(unsolutionText, User("foo"))(engine)
    AddUnsolution("", User("foo"))(engine)

    val response = ListUnsolutions(User("foo"))(engine)

    response should have('texts (List(unsolutionText)))
  }
}
