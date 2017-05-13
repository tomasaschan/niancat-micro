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
        Seq(MessageResponse(notificationChannel, displayNotification(n)))
      case r:Reply =>
        Seq(MessageResponse(receivedInChannel, displayReply(r)))
      }
    }

  private def displayNotification(n: Notification): String = {
    n match {
      case NewPuzzle(p: Puzzle) => {
        val puzzleString = displayPuzzle(p)
        s"Dagens nian är $puzzleString"
      }
    }
  }

  private def displayReply(r: Reply): String = {
    r match {
      case GetReply(p: Puzzle) => displayPuzzle(p)
    }
  }

  private def displayPuzzle(p: Puzzle): String = {
    p.letters grouped(3) mkString(" ")
  }
}