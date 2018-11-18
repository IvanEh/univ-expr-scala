package automata

import automata.MultiMap._

object LexerAutomata {
  import AutomataBuilder.AnyAutomataBuilder

  def translate[M](from: String, to: String, condition: Condition[M]): AutomataBuilder[M] = AutomataBuilder[M]()
    .translate(from, to, condition)

  def translate[M](from: String, to: String, action: Action): AutomataBuilder[M] = AutomataBuilder[M]()
    .translate(from, to, action)

  def translate[M](from: String, to: String): AutomataBuilder[M] = AutomataBuilder[M]()
    .translate(from, to)

  def start[M](initialState: String, initialMemory: M): LexerAutomata[M] = AutomataBuilder[M]()
    .start(initialState, initialMemory)

  def start(initialState: String): LexerAutomata[Unit] = new AnyAutomataBuilder(AutomataBuilder[Unit]())
    .stateless(initialState)
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
                            memory: M,
                            accumulator: String) extends LexerAutomata[M] {

  override def error: Option[String] = None

  def <<(symbol: Symbol): LexerAutomata[M] = {
    val transition = findTransitionFor(symbol)

    transition match {
      case Some(TransitionDestination(nextState, action, _)) =>
        errors.get(nextState) match {
          case Some(errorMessage) => FailedAutomata(nextState, errorMessage)
          case None => this updated (nextState, memory, computeAccumulator(action, symbol))
        }
      case _ => throw new IllegalStateException(s"Exhaustive state transition is not declared for $state")
    }
  }

  private def computeAccumulator(action: Action, symbol: Symbol): String = symbol match {
    case Symbol.Char(char) if action == Accumulate => accumulator + char
    case _ => accumulator
  }

  private def findTransitionFor(symbol: Symbol) = {
    val transition =
      for (stateTransitions <- transitions.get(state).toStream)
        yield stateTransitions find { _.condition accepts (symbol, memory, "") }

    transition.head
  }

  protected final def updated(nextState: String, nextMemory: M, nextAccumulator: String): RunningAutomata[M] = RunningAutomata(nextState, transitions, errors, nextMemory, nextAccumulator)
}

case class FailedAutomata[M] private[automata](override val state: String,
                                       errorMessage: String) extends LexerAutomata[M] {

  override def error: Option[String] = Some(errorMessage)

  override def <<(symbol: Symbol): LexerAutomata[M] =
    throw new IllegalStateException("Automata cannot accept any new symbol in failed state")

}


case class AutomataBuilder[-M](transitions: Map[String, List[TransitionDestination[M]]] = AutomataBuilder.emptyTransitions,
                           errorStates: List[(String, String)] = Nil) {

  def translate[A <: M](from: String, to: String, condition: Condition[A]): AutomataBuilder[A] =
    AutomataBuilder[A](transitions.addBinding(from, condition -> (to, NoAction)))

  def translate(from: String, to: String, action: Action): AutomataBuilder[M] =
    AutomataBuilder(transitions.addBinding(from, Else -> (to, action)))

  def describeError(state: String, errorMessage: String): AutomataBuilder[M] = AutomataBuilder[M](transitions,
     (state, errorMessage) :: errorStates )

  def translate(from: String, to: String): AutomataBuilder[M] =
    AutomataBuilder[M](transitions.addBinding(from, Else -> (to, NoAction)), errorStates)

  def start[Z <: M](initialState: String, initialMemory: Z): LexerAutomata[Z] =
    RunningAutomata[Z](initialState, transitions mapValues { _.sortBy(_.condition)(Condition.ordering) }, errorStates.toMap, initialMemory, "")
}

object AutomataBuilder {
  val emptyTransitions = Map.empty[String, List[TransitionDestination[Any]]]

  implicit class AnyAutomataBuilder(val automataBuilder: AutomataBuilder[Unit]) extends AnyVal {
    def stateless(initialState: String): LexerAutomata[Unit] = automataBuilder.start[Unit](initialState, ())
  }
}
