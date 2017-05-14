package io.github.dandeliondeathray.niancat

object NiancatSlack extends App {
  if (args.size != 3) {
    println(
      """
         Usage: NiancatSlack <token name> <dictionary> <notification channel>
      """.stripMargin)
    System.exit(1)
  }


}