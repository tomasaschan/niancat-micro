package io.github.dandeliondeathray.niancat

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Erik Edin on 2017-05-01.
  */
class PuzzleEngineSpec extends FlatSpec with Matchers {
  "When getting a puzzle which is not set, the engine" should "reply that it's not set" in {
    val engine = new PuzzleEngine()
    val response = Get()(engine)
    response should matchPattern { case NoPuzzleSet() => }
  }
}
