package automata

import com.typesafe.scalalogging.StrictLogging

import scala.collection.mutable
import scalaz.{-\/, \/, \/-}

object Lexer extends StrictLogging {

  def parse[M](expression: String, automata: LexerAutomata[M]): String \/ List[String] = {
    var idx = 0
    val tokens = List.newBuilder[String]
    val symbols = expression + '\0'

    var curr = automata
    logger.debug("->  " + curr.state)

    while (idx < symbols.length()) {
      curr = curr << symbols(idx)
      logger.debug("->  " + curr.state)

      val oldState = curr.state

      if (curr.error.isDefined)
        return -\/(curr.error.get)

      appendTokenIfExists(tokens, curr)

      curr = curr.idle()

      logIdleTransition(curr, oldState)

      if (curr.error.isDefined)
        return -\/(curr.error.get)

      appendTokenIfExists(tokens, curr)

      idx += 1
    }


    \/-(tokens.result())
  }

  private def logIdleTransition[M](current: LexerAutomata[M], oldState: String) = {
    if (current.state != oldState)
      logger.debug("->> " + current.state + " " + (current match { case r: RunningAutomata[M] => r.memory; case _ => ""}))
  }

  private def appendTokenIfExists[M](tokens: mutable.Builder[String, List[String]], curr: LexerAutomata[M]) = {
    if (curr.token.isDefined)
      tokens += curr.token.get
  }
}
