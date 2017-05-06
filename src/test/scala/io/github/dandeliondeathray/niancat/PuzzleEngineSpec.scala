package io.github.dandeliondeathray.niancat

import org.scalatest._
import matchers._

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
class PuzzleEngineSpec extends FlatSpec with Matchers with ResponseMatchers {
  "An engine with no puzzle set" should "reply that no puzzle is set, when asked for the puzzle" in {
    val engine = new PuzzleEngine()
    val response = Get()(engine)
    response shouldBe NoPuzzleSet()
  }

  it should "notify that a new puzzle is set" in {
    val engine = new PuzzleEngine()
    val response = SetPuzzle(Puzzle("ABCDEFGHI"))(engine)

    response should containResponse (NewPuzzle(Puzzle("ABCDEFGHI")))
  }

  it should "store the new puzzle" in {
    val puzzle = Puzzle("ABCDEFGHI")
    val engine = new PuzzleEngine()
    SetPuzzle(puzzle)(engine)

    engine.puzzle shouldBe Some(puzzle)
  }

  "An engine with a puzzle set" should "reply with the puzzle, when asked for the puzzle" in {
    val puzzle = Puzzle("ABCDEFGHI")
    val engine = new PuzzleEngine(Some(puzzle))
    val response = Get()(engine)
    response shouldBe GetReply(puzzle)
  }
}
