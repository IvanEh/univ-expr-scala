package app

import scala.io.StdIn

object App {
  def main(args: Array[String]): Unit = {
    while (true) {
      val input = StdIn.readLine("Enter expression: ")
      val parser = new SimpleExpressionParser(input)
      parser.doLexing()
    }
  }
}
