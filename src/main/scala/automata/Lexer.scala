package automata

import scalaz.{-\/, \/, \/-}

object Lexer {
  def parse[M](expression: String, automata: LexerAutomata[M]): String \/ List[String] = {
    var idx = 0
    val tokens = List.newBuilder[String]
    val symbols = expression + '\0'

    while (idx < symbols.length()) {
      val next = automata << expression(idx)

      if (next.error.isDefined)
        return -\/(next.error.get)

      if (next.token.isDefined)
        tokens += next.token.get

      idx += 1
    }


    \/-(tokens.result())
  }
}
