package io.github.dandeliondeathray.niancat

/**
  * Created by Erik Edin on 2017-04-30.
  */

/** A Puzzle is a String of exactly nine characters. */
case class Puzzle(letters: String)

/** A Word is a potential solution to a Puzzle, but can be any length. */
case class Word(letters: String)

/** A User is of course a user in the chat room. */
case class User(name: String)

class PuzzleEngine(var puzzle: Option[Puzzle] = None) {
  def set(p: Puzzle): Response = DummyReply()
  def get(): Response = {
    puzzle match {
      case None => NoPuzzleSet()
      case Some(p: Puzzle) => GetReply(p)
    }
  }
}

/** A PuzzleCommand is a command sent by a user to Niancat. */
sealed trait PuzzleCommand {
  def apply(engine: PuzzleEngine): Response
}

case class SetPuzzle(puzzle: Puzzle) extends PuzzleCommand {
  def apply(engine: PuzzleEngine): Response = {
    engine.set(puzzle)
  }
}

case class Get() extends PuzzleCommand {
  def apply(engine: PuzzleEngine): Response = engine.get()
}

//case class CheckSolution(word: Word, user: User) extends PuzzleCommand
//case class AddUnsolution(unsolution: String, user: User) extends PuzzleCommand
//case class ListUnsolutions(user: User) extends PuzzleCommand

trait Response
trait Reply extends Response
trait Notification extends Response

case class NoPuzzleSet() extends Reply
case class DummyReply() extends Reply
case class GetReply(puzzle: Puzzle) extends Reply

case class NewPuzzle(puzzle: Puzzle) extends Notification

