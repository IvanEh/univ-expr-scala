package automata

object LexerAutomata {
  def translate(from: String, to: String): AutomataBuilder = AutomataBuilder()
    .translate(from, to)

  def start(initialState: String): LexerAutomata = AutomataBuilder()
    .start(initialState)
}

case class LexerAutomata private(state: String, transitions: Map[String, String]) {
  def accept(symbol: Symbol): LexerAutomata = {
    val nextState = transitions(state)

    LexerAutomata(nextState, transitions)
  }
}

case class AutomataBuilder(transitions: Map[String, String] = Map()) {
  def translate(from: String, to: String): AutomataBuilder = AutomataBuilder(transitions + (from -> to))

  def start(initialState: String): LexerAutomata = LexerAutomata(initialState, transitions)
}
