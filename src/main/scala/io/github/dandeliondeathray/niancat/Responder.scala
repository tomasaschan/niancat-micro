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
      case NewPuzzle(p: Puzzle) => {
        val puzzleString = displayPuzzle(p)
        Seq(MessageResponse(notificationChannel, s"Dagens nian är $puzzleString"))
      }
    }
  }

  private def displayPuzzle(p: Puzzle): String = {
    p.letters grouped(3) mkString(" ")
  }
}