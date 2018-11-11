package automata

object Automata {
  def translate(from: String, to: String): AutomataBuilder = AutomataBuilder()
    .translate(from, to)

  def start(initialState: String): Automata = AutomataBuilder()
    .start(initialState)
}

case class Automata(state: String, transitions: Map[String, String]) {
  def accept(symbol: Symbol): Automata = {
    val nextState = transitions(state)

    Automata(nextState, transitions)
  }
}

case class AutomataBuilder(transitions: Map[String, String] = Map()) {
  def translate(from: String, to: String): AutomataBuilder = AutomataBuilder(transitions + (from -> to))

  def start(initialState: String): Automata = Automata(initialState, transitions)
}
