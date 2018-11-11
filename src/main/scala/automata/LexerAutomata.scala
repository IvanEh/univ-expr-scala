package automata

object LexerAutomata {
  def translate(from: String, to: String): AutomataBuilder = AutomataBuilder()
    .translate(from, to)

  def start(initialState: String): LexerAutomata = AutomataBuilder()
    .start(initialState)
}

sealed abstract class LexerAutomata {
  val state: String
  def error: Option[String]

  def <<(symbol: Symbol): LexerAutomata

  final def isFailed: Boolean = error.isDefined
}

case class RunningAutomata private[automata](override val state: String,
                            transitions: Map[String, String],
                            errors: Map[String, String]) extends LexerAutomata {

  override def error: Option[String] = None

  def <<(symbol: Symbol): LexerAutomata = {
    val nextState = transitions(state)

    errors get nextState match {
      case Some(errorMessage) => FailedAutomata(nextState, errorMessage)
      case None => this withState nextState
    }
  }

  protected def withState(nextState: String): RunningAutomata = RunningAutomata(nextState, transitions, errors)
}

case class FailedAutomata private[automata](override val state: String,
                                       errorMessage: String) extends LexerAutomata {

  override def error: Option[String] = Some(errorMessage)

  override def <<(symbol: Symbol): LexerAutomata = ???

}


case class AutomataBuilder(transitions: Map[String, String] = Map(),
                           errorStates: List[(String, String)] = Nil) {
  def describeError(state: String, errorMessage: String): AutomataBuilder = AutomataBuilder(transitions,
     (state, errorMessage) :: errorStates )

  def translate(from: String, to: String): AutomataBuilder = AutomataBuilder(transitions + (from -> to), errorStates)

  def start(initialState: String): LexerAutomata = RunningAutomata(initialState, transitions, errorStates.toMap)
}
