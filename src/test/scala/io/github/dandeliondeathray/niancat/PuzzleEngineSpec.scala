package io.github.dandeliondeathray.niancat

import org.scalatest._
import Inside._
import matchers._

trait ReplyMatchers {
  class ContainsResponseMatcher(expectedResponse: Response) extends Matcher[Response] {
    def apply(reply: Response) = {
      MatchResult(
        false,
        s"""Reply $reply did not container expected reply $expectedResponse""",
        s"""Reply $reply contained expected reply $expectedResponse"""
      )
    }
  }

  def containResponse(expectedResponse: Response) = new ContainsResponseMatcher(expectedResponse)
}

/**
  * Created by Erik Edin on 2017-05-01.
  */
class PuzzleEngineSpec extends FlatSpec with Matchers with ReplyMatchers {
  "An engine with no puzzle set" should "reply that no puzzle is set, when asked for the puzzle" in {
    val engine = new PuzzleEngine()
    val response = Get()(engine)
    response should matchPattern { case NoPuzzleSet() => }
  }

  it should "notify that a new puzzle is set" in {
    val engine = new PuzzleEngine()
    val response = SetPuzzle(Puzzle("ABCDEFGHI"))(engine)

    response should containResponse (NewPuzzle(Puzzle("ABCDEFGHI")))
  }

  "An engine with a puzzle set" should "reply with the puzzle, when asked for the puzzle" in {
    val puzzle = Puzzle("ABCDEFGHI")
    val engine = new PuzzleEngine(Some(puzzle))
    val response = Get()(engine)
    inside(response) {
      case GetReply(puzzleInReply: Puzzle) => puzzleInReply should be (puzzle)
    }
  }
}
