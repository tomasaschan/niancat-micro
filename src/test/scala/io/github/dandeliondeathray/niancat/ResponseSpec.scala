package io.github.dandeliondeathray.niancat

import org.scalatest._

class ResponseSpec extends FlatSpec with Matchers {
  "NoPuzzleSet" should "say there's no puzzle set" in {
    NoPuzzleSet().toString should include ("Nian är inte satt.")
  }

  "NotInTheDictionary" should "say a word isn't in the dictionary" in {
    NotInTheDictionary(Word("ABC")).toString should include ("ABC finns inte med i SAOL")
  }

  "CorrectSolution" should "say a solution is korrekt" in {
    CorrectSolution(Word("DEF GHI")).toString should include ("DEF GHI är korrekt")
  }

  "YesterdaysPuzzle" should "list all solutions and user names" in {
    val solutionResult = SolutionResult(Map(
      Word("ABCDEFGHI") -> Seq(User("foo"), User("bar")),
      Word("DEFGHIABC") -> Seq(User("baz"))
    ))

    val yesterdaysAsString = YesterdaysPuzzle(solutionResult).toString

    yesterdaysAsString should include ("**ABCDEFGHI**:")
    yesterdaysAsString should include ("**DEFGHIABC**:")
    yesterdaysAsString should include ("foo, bar")
    yesterdaysAsString should include ("baz")
  }

  "MultipleSolutions" should "show the number of solutions" in {
    MultipleSolutions(3).toString should include ("3")
  }
}