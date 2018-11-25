package automata

import com.typesafe.scalalogging.StrictLogging

import scala.collection.mutable
import scalaz.{-\/, \/, \/-}

object Lexer extends StrictLogging {

  def parse[M](expression: String, automata: LexerAutomata[M]): LexerError \/ List[Token] = {
    var idx = 0
    val tokens = List.newBuilder[Token]
    val symbols = expression + '\0'

    var curr = automata
    logger.debug("->  " + curr.state)

    while (idx < symbols.length()) {
      curr = curr << symbols(idx)
      logger.debug("->  " + curr.state)

      val oldState = curr.state

      if (curr.error.isDefined)
        return -\/(LexerError(idx, curr.error.get))

      appendTokenIfExists(tokens, curr)

      curr = curr.idle()

      logIdleTransition(curr, oldState)

      if (curr.error.isDefined)
        return -\/(LexerError(idx, curr.error.get))

      appendTokenIfExists(tokens, curr)

      idx += 1
    }


    \/-(tokens.result())
  }

  private def logIdleTransition[M](current: LexerAutomata[M], oldState: String) = {
    if (current.state != oldState)
      logger.debug("->> " + current.state + " " + (current match { case r: RunningAutomata[M] => r.memory; case _ => ""}))
  }

  private def appendTokenIfExists[M](tokens: mutable.Builder[Token, List[Token]], curr: LexerAutomata[M]) = {
    if (curr.token.isDefined)
      tokens += curr.token.get
  }
}

case class LexerError(position: Int, message: String)
