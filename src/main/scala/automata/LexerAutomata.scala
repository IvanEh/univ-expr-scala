package automata

object LexerAutomata {
  def translate(from: String, to: String): AutomataBuilder = AutomataBuilder()
    .translate(from, to)

  def start(initialState: String): LexerAutomata = AutomataBuilder()
    .start(initialState)
}

class LexerAutomata protected[automata](val state: String,
                            val transitions: Map[String, String],
                            val maybeError: Option[String] = None,
                            val errors: Map[String, String]) {
  def isError: Boolean = maybeError.isDefined

  def accept(symbol: Symbol): LexerAutomata = {
    val nextState = transitions(state)

    errors get nextState match {
      case Some(error) => new FailedLexerAutomata(nextState, transitions, _error = error)
      case None => new LexerAutomata(nextState, transitions, maybeError, errors)
    }
  }
}

class FailedLexerAutomata(state: String,
                          transitions: Map[String, String],
                          _error: String) extends LexerAutomata(state, transitions, Some(_error), Map()) {
  def error: String = maybeError.get
}


case class AutomataBuilder(transitions: Map[String, String] = Map(),
                           errorStates: List[(String, String)] = Nil) {
  def describeError(state: String, errorMessage: String): AutomataBuilder = AutomataBuilder(transitions,
     (state, errorMessage) :: errorStates )

  def translate(from: String, to: String): AutomataBuilder = AutomataBuilder(transitions + (from -> to), errorStates)

  def start(initialState: String): LexerAutomata = new LexerAutomata(initialState, transitions, None, errorStates.toMap)
}
