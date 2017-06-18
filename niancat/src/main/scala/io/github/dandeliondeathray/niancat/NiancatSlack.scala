package io.github.dandeliondeathray.niancat

import akka.actor.ActorSystem
import slack.rtm.SlackRtmClient
import scala.concurrent.duration._

class NiancatSlack(val token: String,
                   val dictionary: Dictionary,
                   notificationChannel: Channel)(implicit val system: ActorSystem) {
  implicit val ec = system.dispatcher

  val client = SlackRtmClient(token, 20.seconds)
  val selfUser = User(client.state.self.name)

  val puzzleSolution = new DictionaryPuzzleSolution(dictionary)
  val parser = new SlackParser
  val engine = new PuzzleEngine(dictionary, puzzleSolution)
  val responder = new SwedishResponder(notificationChannel)

  val sendMessage: (Channel, String) => Unit = (channel, msg) => client.sendMessage(channel.id, msg)

  val messageHandler = new MessageHandler(parser, engine, responder, sendMessage, selfUser)

  client.onMessage { message =>
    println(s"onMessage: $message")
    client.state.getUserById(message.user).foreach(user =>
      messageHandler.handle(Channel(message.channel), User(user.name), message.text))
  }

  client.onEvent { event =>
    println(s"Event: $event")
  }
}