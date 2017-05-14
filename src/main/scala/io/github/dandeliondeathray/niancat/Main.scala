package io.github.dandeliondeathray.niancat

import akka.actor.ActorSystem
import slack.api.BlockingSlackApiClient
import slack.models

import scala.io.Source
import scala.util.{Failure, Success, Try}

object NiancatSlackApp extends App {
  if (args.size != 3) {
    println(
      """
         Usage: NiancatSlack <token name> <dictionary> <notification channel>
      """.stripMargin)
    System.exit(1)
  }

  val tokenName = args(0)
  val dictionaryFile = args(1)
  val notificationChannelName = args(2)

  val tryToken = Token.read(tokenName)

  val token: String = tryToken match {
    case Failure(e) => {
      println(s"Could not read token $tokenName: Got exception $e")
      System.exit(1)
      ""
    }
    case Success(t: String) => t.stripLineEnd
  }

  println(s"Using token '$token'")

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

  implicit val system = ActorSystem("slack")
  val client = BlockingSlackApiClient(token)
  val maybeNotificationChannel: Option[models.Channel] = client.listChannels().filter(_.name == notificationChannelName).headOption
  val notificationChannel: Channel = maybeNotificationChannel match {
    case None => {
      println(s"No such channel $notificationChannelName")
      System.exit(1)
      Channel("")
    }
    case Some(c: models.Channel) => Channel(c.id)
  }

  new NiancatSlack(token, dictionary, notificationChannel)
}