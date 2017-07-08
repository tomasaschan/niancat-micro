package io.github.dandeliondeathray.niancat

import io.circe._
import org.http4s._
import org.http4s.dsl._
import org.http4s.circe._

object NiancatService {


  //messageHandler.handle(Channel(message.channel), User(user.name), message.text))

  def service(dictionary: Dictionary) = {
    val puzzleSolution = new DictionaryPuzzleSolution(dictionary)
    val engine = new PuzzleEngine(dictionary, puzzleSolution)
    //val responder = new SwedishResponder()
    //val messageHandler = new MessageHandler(engine, responder)

    HttpService {

      case GET -> Root / "puzzle"  =>
        Ok(Json.obj("message" -> Json.fromString(s"Hello")))
    }
  }
}
