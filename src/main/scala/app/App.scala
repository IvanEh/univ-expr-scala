package app

import automata.{LexerError, Token}

import scala.io.StdIn
import scalaz.{-\/, \/-}

object App {

  def main(args: Array[String]): Unit = {
    while (true) {
      val input = StdIn.readLine("Enter expression: ")
      val parser = new SimpleExpressionParser(input)
      val result = parser.doLexing()
      result match {
        case -\/(LexerError(pos, message)) => printError(input, pos, message)
        case \/-(tokens) => printTokens(tokens)
      }
    }
  }

  private def printError(expr: String, pos: Int, error: String): Unit = {
    println(expr)
    println("^".padTo(pos + 1, " ").mkString.reverse + Console.RED + s" - $error" + Console.BLACK)
  }

  private def printTokens(tokens: List[Token]): Unit = {
    print(Console.GREEN)
    println("Expression parsed successfully")
    print(Console.BLACK)
    println(tokens.mkString(","))
  }
}
