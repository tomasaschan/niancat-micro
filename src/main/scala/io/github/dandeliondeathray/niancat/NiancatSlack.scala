package io.github.dandeliondeathray.niancat

import akka.actor.ActorSystem
import slack.rtm.SlackRtmClient

class NiancatSlack(val token: String, val dictionary: Dictionary, notificationChannel: Channel) {
  implicit val system = ActorSystem("slack")
  implicit val ec = system.dispatcher

  val client = SlackRtmClient(token)
  val selfId = client.state.self.id

  val puzzleSolution = new DictionaryPuzzleSolution(dictionary)
  val parser = new SlackParser
  val engine = new PuzzleEngine(dictionary, puzzleSolution)
  val responder = new SwedishResponder(notificationChannel)

  val sendMessage: (Channel, String) => Unit = (channel, msg) => client.sendMessage(channel.id, msg)

  val messageHandler = new MessageHandler(parser, engine, responder, sendMessage)

  client.onMessage { message =>
    client.state.getUserById(message.user).foreach(user =>
      messageHandler.handle(Channel(message.channel), User(user.name), message.text))
  }
}