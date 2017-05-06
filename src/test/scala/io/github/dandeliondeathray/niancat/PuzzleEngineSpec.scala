package io.github.dandeliondeathray.niancat

import org.scalatest._
import Inside._

/**
  * Created by Erik Edin on 2017-05-01.
  */
class PuzzleEngineSpec extends FlatSpec with Matchers {
  "When getting a puzzle which is not set, the engine" should "reply that it's not set" in {
    val engine = new PuzzleEngine()
    val response = Get()(engine)
    response should matchPattern { case NoPuzzleSet() => }
  }

  "When getting a puzzle which is set, the engine" should "reply with the puzzle" in {
    val puzzle = Puzzle("ABCDEFGHI")
    val engine = new PuzzleEngine(Some(puzzle))
    val response = Get()(engine)
    inside(response) {
      case GetReply(puzzleInReply: Puzzle) => puzzleInReply should be (puzzle)
    }
  }
}
