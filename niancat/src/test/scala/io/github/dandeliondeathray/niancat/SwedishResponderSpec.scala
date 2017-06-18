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

  it should "reply with the puzzle for a Get response" in {
    val msgResponses =
      swedishResponder.messageResponses(GetReply(Puzzle("ABCDEFGHI")), privateChannel)

    msgResponses.loneElement.channel shouldBe (privateChannel)
    msgResponses.loneElement.msg shouldBe "ABC DEF GHI"
  }

  it should "ignore a NoResponse" in {
    val msgResponses = swedishResponder.messageResponses(NoResponse(), privateChannel)

    msgResponses shouldBe Seq()
  }

  it should "return a list of responses if it's a composite response" in {
    val msgResponses = swedishResponder.messageResponses(
      CompositeResponse(Vector(NoPuzzleSet(), MultipleSolutions(3))), privateChannel
    )

    msgResponses(0) should have ('channel (privateChannel))
    msgResponses(1) should have ('channel (notificationChannel))
  }

  "a Channel" should "be private if it starts with D" in {
    Channel("D01234").visibility shouldBe PrivateChannel()
  }

  it should "be public if it doesn't start with a D" in {
    Channel("C01345").visibility shouldBe PublicChannel()
  }
}