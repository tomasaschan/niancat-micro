package io.github.dandeliondeathray.niancat

import scala.util.Try
import io.circe._
import io.circe.syntax._
import io.circe.parser.decode
import io.circe.generic.auto._
import java.io.File
import java.io.FileWriter
import java.io.FileOutputStream
import java.io.BufferedWriter
import java.io.FileReader
import java.io.BufferedReader
import java.io.StringReader
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption._
import java.nio.file.StandardOpenOption

abstract trait StorableState {
  def save(): Unit
  def load(): Unit
}

class InMemoryState extends StateRepr with StorableState {
  var puzzle: Option[Puzzle] = None
  var unsolutions: Map[User, Seq[String]] = Map()
  var unconfirmedUnsolutions: Map[User, String] = Map()
  var solutions: Seq[(Word, User)] = Seq()
  var streaks: Map[User, Int] = Map()
  def save() = { /*noop*/ }
  def load() = { /*noop*/ }
}

class JsonFileBackedState(val filePath: String) extends StateRepr with StorableState {
  var puzzle: Option[Puzzle] = None
  var unsolutions: Map[User, Seq[String]] = Map()
  var unconfirmedUnsolutions: Map[User, String] = Map()
  var solutions: Seq[(Word, User)] = Seq()
  var streaks: Map[User, Int] = Map()

  private def toStringMap[T](m: Map[User, T]): Map[String, T] = m map { case (k, v)   => k.name -> v }
  private def fromStringMap[T](m: Map[String, T]): Map[User, T] = m map { case (k, v) => User(k) -> v }
  implicit val stateEncoder: Encoder[JsonFileBackedState] = new Encoder[JsonFileBackedState] {
    final def apply(s: JsonFileBackedState): Json = Json.obj(
      ("puzzle", puzzle.asJson),
      ("unsolutions", toStringMap(unsolutions).asJson),
      ("unconfirmedUnsolutions", toStringMap(unconfirmedUnsolutions).asJson),
      ("solutions", solutions.asJson),
      ("streaks", toStringMap(streaks).asJson)
    )
  }

  implicit val stateDecoder: Decoder[JsonFileBackedState] = new Decoder[JsonFileBackedState] {
    final def apply(c: HCursor): Decoder.Result[JsonFileBackedState] = {
      for {
        pzzl <- c.downField("puzzle").as[Option[Puzzle]]
        sltns <- c.downField("solutions").as[Seq[(Word, User)]]
        nsltns <- c.downField("unsolutions").as[Map[String, Seq[String]]] map fromStringMap
        ncnfrmdNslts <- c.downField("unconfirmedUnsolutions").as[Map[String, String]] map fromStringMap
        strks <- c.downField("streaks").as[Map[String, Int]] map fromStringMap
      } yield {
        val state = new JsonFileBackedState(filePath)
        state.puzzle = pzzl
        state.solutions = sltns
        state.unsolutions = nsltns
        state.unconfirmedUnsolutions = ncnfrmdNslts
        state.streaks = strks
        state
      }
    }
  }

  def save(): Unit = {
    val writer =
      Files.newBufferedWriter(Paths.get(filePath), StandardCharsets.UTF_8, CREATE, TRUNCATE_EXISTING)
    writer.write(this.asJson.toString)
    writer.flush()
    writer.close()
  }
  def load(): Unit = {
    Some(filePath)
      .map(Paths.get(_))
      .filter(Files.exists(_))
      .map(path => new String(Files.readAllBytes(path), StandardCharsets.UTF_8))
      .flatMap(read) match {
      case Some(decoded) => apply(decoded)
      case None          => backup(filePath)
    }
  }

  private def read(json: String): Option[JsonFileBackedState] = {
    decode[JsonFileBackedState](json) match {
      case Left(ex) => {
        println("ERROR: Parsing state file failed.")
        println(s"STATE FILE CONTENTS:\n${json}")
        println(ex)
        None
      }
      case Right(state) => Some(state)
    }
  }
  private def apply(decoded: JsonFileBackedState) = {
    puzzle = decoded.puzzle
    solutions = decoded.solutions
    streaks = decoded.streaks
    unsolutions = decoded.unsolutions
    unconfirmedUnsolutions = decoded.unconfirmedUnsolutions
  }
  private def backup(filePath: String) = {
    println(s"WARNING: File ${filePath} could not be parsed to a state; restarting with a blank slate.")
    Try(if (Files.exists(Paths.get(filePath))) {
      println(s"Moving ${filePath} to ${filePath}.bak to avoid overriding an old state...")
      Files.move(Paths.get(filePath), Paths.get(s"${filePath}.bak"))
      println("Previous state backed up.")
    }).toEither match {
      case Left(ex) => {
        println("ERROR: Backup failed!")
        println(ex)
      }
      case Right(_) => {}
    }
  }
}
