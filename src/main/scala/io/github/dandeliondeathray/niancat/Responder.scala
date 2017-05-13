package io.github.dandeliondeathray.niancat

case class Channel(id: String) {
  def visibility: ChannelVisibility = {
    if (id startsWith("D")) {
      PrivateChannel()
    } else {
      PublicChannel()
    }
  }
}
case class MessageResponse(channel: Channel, msg: String)

trait Responder {
  def messageResponses(response: Response,
                       receivedInChannel: Channel): Seq[MessageResponse]
}

class SwedishResponder(val notificationChannel: Channel) extends Responder {
  def messageResponses(response: Response,
                       receivedInChannel: Channel): Seq[MessageResponse] = {
    response match {
      case n:Notification =>
        Seq(MessageResponse(notificationChannel, n toString()))
      case r:Reply =>
        Seq(MessageResponse(receivedInChannel, r toString()))
      case NoResponse() => Seq()
      case CompositeResponse(v: Vector[Response]) =>
        v flatMap (messageResponses(_, receivedInChannel))
      }
    }
}