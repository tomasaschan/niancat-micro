package io.github.dandeliondeathray.niancat

import io.circe._
import org.http4s._
import org.http4s.dsl._
import org.http4s.circe._

object NiancatService {
  def service(dictionary: Dictionary) = {
    val puzzleSolution = new DictionaryPuzzleSolution(dictionary)
    val engine = new PuzzleEngine(dictionary, puzzleSolution)
    val responder = new NiancatApiResponder()

    HttpService {

      case GET -> Root / "v1" / "puzzle"  => {
        val command = Get()
        val response = command(engine)
        val messageResponses = responder.messageResponses(response)
        Ok(Json.fromValues(messageResponses map (_.toJSON)))
      }
      case req @ POST -> Root / "v1" / "puzzle" => {
        Ok(Json.fromString("ABX"))
      }
    }
  }
}
