package automata

import automata.MultiMap._

object LexerAutomata {

  def translate[M](from: String, to: String, condition: Condition[M]): AutomataBuilder[M] = AutomataBuilder[M]()
    .translate(from, to, condition)

  def translate[M](from: String, to: String): AutomataBuilder[M] = AutomataBuilder[M]()
    .translate(from, to)

  def start[M](initialState: String, initialMemory: M): LexerAutomata[M] = AutomataBuilder[M]()
    .start(initialState, initialMemory)
}

sealed abstract class LexerAutomata[M] {
  val state: String
  def error: Option[String]

  def <<(symbol: Symbol): LexerAutomata[M]

  final def isFailed: Boolean = error.isDefined
}

case class RunningAutomata[M] private[automata](override val state: String,
                            transitions: Map[String, List[TransitionDestination[M]]],
                            errors: Map[String, String],
                            memory: M) extends LexerAutomata[M] {

  override def error: Option[String] = None

  def <<(symbol: Symbol): LexerAutomata[M] = {
    val transition = findTransitionFor(symbol)

    transition match {
      case Some(TransitionDestination(nextState, _)) =>
        errors.get(nextState) match {
          case Some(errorMessage) => FailedAutomata(nextState, errorMessage)
          case None => RunningAutomata(nextState, transitions, errors, memory)
        }
      case _ => throw new IllegalStateException(s"Exhaustive state transition is not declared for $state")
    }
  }

  private def findTransitionFor(symbol: Symbol) = {
    val transition =
      for (stateTransitions <- transitions.get(state).toStream)
        yield stateTransitions find { _.condition accepts (symbol, memory, "") }

    transition.head
  }

  protected def withState(nextState: String): RunningAutomata[M] = RunningAutomata(nextState, transitions, errors, memory)
}

case class FailedAutomata[M] private[automata](override val state: String,
                                       errorMessage: String) extends LexerAutomata[M] {

  override def error: Option[String] = Some(errorMessage)

  override def <<(symbol: Symbol): LexerAutomata[M] =
    throw new IllegalStateException("Automata cannot accept any new symbol in failed state")

}


case class AutomataBuilder[M](transitions: Map[String, List[TransitionDestination[M]]] = Map.empty[String, List[TransitionDestination[M]]],
                           errorStates: List[(String, String)] = Nil) {
  def translate(from: String, to: String, condition: Condition[M]): AutomataBuilder[M] =
    AutomataBuilder[M](transitions.addBinding(from, to -> condition))

  def describeError(state: String, errorMessage: String): AutomataBuilder[M] = AutomataBuilder[M](transitions,
     (state, errorMessage) :: errorStates )

  def translate(from: String, to: String): AutomataBuilder[M] =
    AutomataBuilder[M](transitions.addBinding(from, to -> Else), errorStates)

  def start(initialState: String, initialMemory: M): LexerAutomata[M] =
    RunningAutomata[M](initialState, transitions mapValues { _.sortBy(_.condition)(Condition.ordering) }, errorStates.toMap, initialMemory)
}
