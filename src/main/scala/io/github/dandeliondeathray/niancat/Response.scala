package io.github.dandeliondeathray.niancat

object DisplayHelper {
  implicit class PuzzleDisplay(p: Puzzle) {
    def show: String = p.letters grouped(3) mkString(" ")
  }

  implicit class SolutionResultDisplay(s: SolutionResult) {
    def display: Seq[String] = s.wordsAndSolvers.map(kv => showWordAndSolution(kv._1, kv._2)) toSeq

    private def showWordAndSolution(w: Word, solvers: Seq[User]): String = {
      s"**${w.letters}**: " ++ solvers.map(_.name).mkString(", ")
    }
  }
}

import DisplayHelper._

sealed trait Response
sealed trait Reply extends Response
sealed trait Notification extends Response

case class CompositeResponse(responses: Vector[Response]) extends Response
case class NoResponse() extends Response

case class NoPuzzleSet() extends Reply {
  override def toString: String = "Nian är inte satt."
}
case class GetReply(puzzle: Puzzle) extends Reply {
  override def toString: String = puzzle show
}
case class NotInTheDictionary(word: Word) extends Reply {
  override def toString: String = s"Ordet ${word.letters} finns inte med i SAOL."
}
case class CorrectSolution(word: Word) extends Reply {
  override def toString: String = s"Ordet ${word.letters} är korrekt!"
}
case class WordAndPuzzleMismatch(word: Word, puzzle: Puzzle) extends Reply {
  override def toString: String = s"Ordet ${word.letters} matchar inte nian ${puzzle show}"
}
case class IncorrectLength(word: Word) extends Reply {
  override def toString: String = s"${word.letters} är inte nio tecken långt."
}
case class InvalidPuzzle(puzzle: Puzzle) extends Reply {
  override def toString: String = s"Pusslet ${puzzle show} är inte giltigt!"
}
case class InvalidCommandReply(msg: String, invalidCommandError: InvalidCommandError) extends Reply {
  override def toString = {
    invalidCommandError match {
      case UnknownCommand(cmd: String) => s"Okänt kommando $cmd"
      case WrongArguments(actual: Int, expected: Int) =>
        s"Fel antal argument. Fick $actual men förväntade $expected"
    }
  }
}
case class SamePuzzle(puzzle: Puzzle) extends Reply {
  override def toString = s"Pusslet ${puzzle show} är redan satt!"
}

case class NewPuzzle(puzzle: Puzzle) extends Notification {
  override def toString: String = {
    s"Dagens nian är ${puzzle show}"
  }
}
case class YesterdaysPuzzle(result: SolutionResult) extends Notification {
  override def toString: String = {
    val lines = Seq("**Gårdagens lösningar:**") ++ result.display

    lines mkString("\n")
  }
}
case class MultipleSolutions(n: Int) extends Notification {
  override def toString = "Dagens nian har $n lösningar."
}

case class SolutionNotification(user: User) extends Notification {
  override def toString = s"${user.name} löste nian!"
}