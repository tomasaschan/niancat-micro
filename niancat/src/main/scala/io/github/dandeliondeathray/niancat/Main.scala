package io.github.dandeliondeathray.niancat

import scala.io.Source
import scala.util.{Failure, Success, Try}

object NiancatSlackApp extends App {
  if (args.size == 2 && args(0) == "filter") {
    filter(args(1))
    System.exit(0)
  }

  if (args.size != 0) {
    println(
      """
         Usage: niancat
                    or
                niancat filter <dictionary>
      """.stripMargin)
    System.exit(1)
  }

  val dictionaryFile = args(1)

  val tryDictionary: Try[Dictionary] = Try {
    val lines = Source.fromFile(dictionaryFile).getLines().toSeq
    new NineLetterDictionary(lines)
  }

  val dictionary: Dictionary = tryDictionary match {
    case Failure(e) => {
      println(s"Could not read dictionary in file $dictionaryFile: Got exception $e")
      System.exit(1)
      NineLetterDictionary()
    }
    case Success(d: Dictionary) => d
  }



  def filter(dictionaryFile: String): Unit = {
    val maybeDictionary: Try[NineLetterDictionary] = Try {
      val lines = Source.fromFile(dictionaryFile).getLines().toSeq
      new NineLetterDictionary(lines)
    }

    val filtered: Dictionary = maybeDictionary match {
      case Failure(e) => {
        println(s"Could not read dictionary in file $dictionaryFile: Got exception $e")
        System.exit(1)
        NineLetterDictionary()
      }
      case Success(d: NineLetterDictionary) => d
    }

    (filtered.toSeq map ((w: Word) => w.letters) sorted) foreach (println(_))
  }
}