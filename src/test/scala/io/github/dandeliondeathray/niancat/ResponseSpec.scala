package io.github.dandeliondeathray.niancat

import org.scalatest._

class ResponseSpec extends FlatSpec with Matchers {
  "NoPuzzleSet" should "say there's no puzzle set" in {
    NoPuzzleSet().toResponse should include ("Nian är inte satt.")
  }

  "NotInTheDictionary" should "say a word isn't in the dictionary" in {
    NotInTheDictionary(Word("ABC")).toResponse should include ("ABC finns inte med i SAOL")
  }

  "CorrectSolution" should "say a solution is korrekt" in {
    CorrectSolution(Word("DEF GHI")).toResponse should include ("DEF GHI är korrekt")
  }

  "YesterdaysPuzzle" should "list all solutions and user names" in {
    val solutionResult = SolutionResult(Map(
      Word("ABCDEFGHI") -> Seq(User("foo"), User("bar")),
      Word("DEFGHIABC") -> Seq(User("baz"))
    ))

    val yesterdaysAsString = YesterdaysPuzzle(solutionResult).toResponse

    yesterdaysAsString should include ("*ABCDEFGHI*:")
    yesterdaysAsString should include ("*DEFGHIABC*:")
    yesterdaysAsString should include ("foo, bar")
    yesterdaysAsString should include ("baz")
  }

  "MultipleSolutions" should "show the number of solutions" in {
    MultipleSolutions(3).toResponse should include ("3")
  }

  "Solution notification" should "not include a solution id if not set" in {
    SolutionNotification(User("foo"), None).toResponse should not (include ("ord "))
  }

  it should "include id if set" in {
    SolutionNotification(User("foo"), Some(42)).toResponse should include ("ord 42")
  }

  "All unsolutions" should "contain the names of users and the texts" in {
    val text1 = "text1"
    val text2 = "text2"
    val text3 = "text3"

    val allUnsolutionsResponse = AllUnsolutions(Map(
      User("foo") -> List(text1, text2),
      User("bar") -> List(text3))).toResponse

    allUnsolutionsResponse should include ("foo")
    allUnsolutionsResponse should include ("bar")
    allUnsolutionsResponse should include (text1)
    allUnsolutionsResponse should include (text2)
    allUnsolutionsResponse should include (text3)
  }
}