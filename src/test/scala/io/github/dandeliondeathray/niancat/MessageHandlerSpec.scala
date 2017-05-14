package io.github.dandeliondeathray.niancat

import org.scalatest._
import org.scalamock.scalatest._

class MessageHandlerSpec extends FlatSpec with Matchers with MockFactory {
  "MessageHandler" should "send replies and notifications when sending commands" in {
    val privateChannel = Channel("D01234")
    val notificationChannel = Channel("C12345")
    val user = User("foo")

    val dictionary = NineLetterDictionary("ABCDEFGHI")
    val puzzleSolution = new DictionaryPuzzleSolution(dictionary)

    val parser = new SlackParser
    val engine = new PuzzleEngine(dictionary, puzzleSolution)
    val responder = new SwedishResponder(notificationChannel)

    val sendMessage = mockFunction[Channel, String, Unit]
    // One notification when setting the puzzle.
    sendMessage expects(notificationChannel, *) atLeastOnce()
    // One reply when getting the puzzle
    sendMessage expects(privateChannel, *)


    val messageHandler = new MessageHandler(parser, engine, responder, sendMessage)

    messageHandler.handle(privateChannel, user, "!setnian DEFGHIABC")
    messageHandler.handle(privateChannel, user, "!nian")
  }
}