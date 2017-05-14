package io.github.dandeliondeathray.niancat

class MessageHandler(val parser: Parser,
                     val engine: PuzzleEngine,
                     val responder: Responder,
                     val sendMessage: (Channel, String) => Unit) {

  def handle(channel: Channel, user: User, message: String): Unit = {
    val command = parser.parse(message, user, channel.visibility)
    val response = command(engine)
    val messageResponses = responder.messageResponses(response, channel)

    messageResponses foreach (m => sendMessage(m.channel, m.msg))
  }
}