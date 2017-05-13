package io.github.dandeliondeathray.niancat

case class Channel(id: String)
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
      }
    }
}