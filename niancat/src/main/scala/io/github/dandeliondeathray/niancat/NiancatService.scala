package io.github.dandeliondeathray.niancat

import io.circe._
import org.http4s._
import org.http4s.dsl._
import org.http4s.circe._
import io.circe.generic.auto._

case class SetPuzzleBody(puzzle: String)
case class CheckSolutionBody(user: String, solution: String)
case class AddUnsolutionBody(unsolution: String)

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
        req.as(jsonOf[SetPuzzleBody]) flatMap { setPuzzleBody =>
          val command = SetPuzzle(Puzzle(setPuzzleBody.puzzle))
          val response = command(engine)
          val messageResponses = responder.messageResponses(response)
          Ok(Json.fromValues(messageResponses map (_.toJSON)))
        }
      }
      case req @ POST -> Root / "v1" / "solution" => {
        req.as(jsonOf[CheckSolutionBody]) flatMap { checkSolutionBody =>
          val command = CheckSolution(Word(checkSolutionBody.solution), User(checkSolutionBody.user))
          val response = command(engine)
          val messageResponses = responder.messageResponses(response)
          Ok(Json.fromValues(messageResponses map (_.toJSON)))
        }
      }
      case GET -> Root / "v1" / "unsolution" / user => {
        val command = ListUnsolutions(User(user))
        val response = command(engine)
        val messageResponses = responder.messageResponses(response)
        Ok(Json.fromValues(messageResponses map (_.toJSON)))
      }
      case req @ POST -> Root / "v1" / "unsolution" / user => {
        req.as(jsonOf[AddUnsolutionBody]) flatMap { addUnsolutionBody =>
          val command = AddUnsolution(addUnsolutionBody.unsolution, User(user))
          val response = command(engine)
          val messageResponses = responder.messageResponses(response)
          Ok(Json.fromValues(messageResponses map (_.toJSON)))
        }
      }
    }
  }

}
