package io.github.dandeliondeathray.niancat

import org.scalatest._
import LoneElement._

class SwedishResponderSpec extends FlatSpec with Matchers {
  def notificationChannel: Channel = Channel("C0123456")
  def privateChannel: Channel = Channel("D0123456")
  def swedishResponder: SwedishResponder = new SwedishResponder(notificationChannel)

  "a SwedishResponder" should "notify when a new puzzle is set" in {
    val msgResponses =
      swedishResponder.messageResponses(NewPuzzle(Puzzle("ABCDEFGHI")), privateChannel)

    msgResponses.loneElement should have ('channel (notificationChannel))
  }

  it should "respond with the puzzle for a NewPuzzle response" in {
    val msgResponses =
      swedishResponder.messageResponses(NewPuzzle(Puzzle("ABCDEFGHI")), privateChannel)

    msgResponses.loneElement.msg should include ("ABC DEF GHI")
  }
}