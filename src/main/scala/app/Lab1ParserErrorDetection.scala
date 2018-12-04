package app

import automata.{LexerError, Token}

import scala.io.StdIn
import scalaz.{-\/, \/-}

object Lab1ParserErrorDetection {

  def main(args: Array[String]): Unit = {
    while (true) {
      val input = StdIn.readLine("Enter expression: ")
      val parser = new SimpleExpressionParser(input, List("sin", "cos"))
      val lexResult = parser.doLexing()

      lexResult match {
        case -\/(LexerError(pos, message)) => printError(input, pos, message)
        case \/-(tokens) =>
          doSemAnalysis(parser, tokens)
      }
    }
  }

  private def doSemAnalysis(parser: SimpleExpressionParser, tokens: List[Token]) = {
    val semResult = parser.doSemAnalysis()
    semResult match {
      case Some(semError) => println(Console.RED + semError + Console.BLACK)
      case None => printTokens(tokens)
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
