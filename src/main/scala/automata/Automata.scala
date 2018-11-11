package automata

object Automata {
  def start(initialState: String): Automata = Automata(state = initialState)
}

case class Automata(state: String)
