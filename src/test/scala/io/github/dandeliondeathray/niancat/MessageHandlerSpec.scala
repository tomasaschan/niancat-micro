package io.github.dandeliondeathray.niancat

import org.scalatest._
import org.scalamock.scalatest._

class MessageHandlerSpec extends FlatSpec with Matchers with MockFactory {
  "MessageHandler" should "send replies and notifications when sending commands" in {
    val botUser = User("thisbot")

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


    val messageHandler = new MessageHandler(parser, engine, responder, sendMessage, botUser)

    messageHandler.handle(privateChannel, user, "!setnian DEFGHIABC")
    messageHandler.handle(privateChannel, user, "!nian")
  }

  it should "ignore messages from itself" in {
    val botUser = User("thisbot")

    val privateChannel = Channel("D01234")
    val notificationChannel = Channel("C12345")

    val dictionary = NineLetterDictionary("ABCDEFGHI")
    val puzzleSolution = new DictionaryPuzzleSolution(dictionary)

    val parser = new SlackParser
    val engine = new PuzzleEngine(dictionary, puzzleSolution)
    val responder = new SwedishResponder(notificationChannel)

    val sendMessage = mockFunction[Channel, String, Unit]
    // No message is sent
    sendMessage expects(*, *) never()

    val messageHandler = new MessageHandler(parser, engine, responder, sendMessage, botUser)

    messageHandler.handle(privateChannel, botUser, "!setnian DEFGHIABC")
    messageHandler.handle(privateChannel, botUser, "!nian")
    messageHandler.handle(privateChannel, botUser, "This is not a solution")

  }
}