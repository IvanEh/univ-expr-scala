package automata

import automata.MultiMap._

object LexerAutomata {

  import AutomataBuilder.AnyAutomataBuilder

  def translate[M](from: String, to: String, condition: Condition[M]): AutomataBuilder[M] = AutomataBuilder[M]()
    .translate(from, to, condition)

  def translate[M](from: String, to: String, action: Action[M]): AutomataBuilder[M] = AutomataBuilder[M]()
    .translate(from, to, action)

  def translate[M](from: String, to: String, condition: Condition[M], action: Action[M]): AutomataBuilder[M]
    = AutomataBuilder[M]().translate(from, to, condition, action)

  def translate[M](from: String, to: String): AutomataBuilder[M] = AutomataBuilder[M]()
    .translate(from, to)

  def start[M](initialState: String, initialMemory: M): LexerAutomata[M] = AutomataBuilder[M]()
    .start(initialState, initialMemory)

  def start(initialState: String): LexerAutomata[Unit] = new AnyAutomataBuilder(AutomataBuilder[Unit]())
    .stateless(initialState)
}

sealed abstract class LexerAutomata[M] {
  val state: String
  val accumulator: String
  val token: Option[Token]
  val error: Option[String]

  def <<(symbol: Symbol): LexerAutomata[M]
  def terminate(): LexerAutomata[M] = this << Symbol.Terminal
  def idle(): LexerAutomata[M] = this << Symbol.None // TODO: do idle() when needed automatically

  final def isFailed: Boolean = error.isDefined
}

case class RunningAutomata[M] private[automata](override val state: String,
                            transitions: Map[String, List[TransitionDestination[M]]],
                            errors: Map[String, String],
                            memory: M,
                            override val accumulator: String,
                            override val token: Option[Token]) extends LexerAutomata[M] {

  override val error: Option[String] = None

  def <<(symbol: Symbol): LexerAutomata[M] = {
    val transition = findTransitionFor(symbol)

    transition match {
      case Some(TransitionDestination(nextState, action, _)) =>
        errors.get(nextState) match {
          case Some(errorMessage) => FailedAutomata(nextState, errorMessage)
          case None => this updated (nextState = nextState,
            nextMemory = action.nextMemory(memory).asInstanceOf[M],
            nextToken = computeToken(action, symbol),
            nextAccumulator = computeAccumulator(action, symbol))
        }
      case _ => throw new IllegalStateException(s"Exhaustive state transition is not declared for $state")
    }
  }

  private def computeAccumulator(action: Action[M], symbol: Symbol): String = action.pushToken match {
    case Some(_) => computePartialAccumulator(action, symbol, "")
    case None => computePartialAccumulator(action, symbol, accumulator)
  }

  private def computePartialAccumulator(action: Action[M], symbol: Symbol, accumulator: String): String = symbol match {
    case Symbol.Char(char) if action.accumulate => accumulator + char
    case _ => accumulator
  }

  private def computeToken(action: Action[M], symbol: Symbol): Option[Token] = action.pushToken match {
    case Some(marker) => Some (Token(accumulator, marker))
    case None => None
  }

  private def findTransitionFor(symbol: Symbol) = {
    val transition =
      for (stateTransitions <- transitions.get(state).toStream)
        yield stateTransitions find { _.condition accepts (symbol, memory, "") }

    transition.headOption getOrElse None match {
      case None if symbol == Symbol.None => Some(TransitionDestination[M](state, NoAction, Else))
      case t => t
    }
  }

  protected final
  def updated(nextState: String, nextMemory: M, nextAccumulator: String, nextToken: Option[Token]): RunningAutomata[M]
    = RunningAutomata(nextState, transitions, errors, nextMemory, nextAccumulator, nextToken)
}

case class FailedAutomata[M] private[automata](override val state: String,
                                       errorMessage: String) extends LexerAutomata[M] {

  override val accumulator: String = ""
  override val token: Option[Token] = None
  override val error: Option[String] = Some(errorMessage)

  override def <<(symbol: Symbol): LexerAutomata[M] =
    throw new IllegalStateException("Automata cannot accept any new symbol in failed state")

}


case class AutomataBuilder[-M](transitions: Map[String, List[TransitionDestination[M]]] = AutomataBuilder.emptyTransitions,
                           errorStates: List[(String, String)] = Nil) {


  def translate[A <: M](from: String, to: String, condition: Condition[A]): AutomataBuilder[A] =
    AutomataBuilder[A](transitions.addBinding(from, condition -> (to, NoAction)))

  def translate[A <: M](from: String, to: String, action: Action[A]): AutomataBuilder[A] =
    AutomataBuilder(transitions.addBinding(from, Else -> (to, action)))

  def translate[A <: M](from: String, to: String, condition: Condition[A], action: Action[A]): AutomataBuilder[A] =
    AutomataBuilder(transitions.addBinding(from, condition -> (to, action)))

  def describeError(state: String, errorMessage: String): AutomataBuilder[M] = AutomataBuilder[M](transitions,
     (state, errorMessage) :: errorStates )

  def translate(from: String, to: String): AutomataBuilder[M] =
    AutomataBuilder[M](transitions.addBinding(from, Else -> (to, NoAction)), errorStates)

  def start[Z <: M](initialState: String, initialMemory: Z): LexerAutomata[Z] =
    RunningAutomata[Z](initialState, transitions mapValues { _.sortBy(_.condition)(Condition.ordering) }, errorStates.toMap, initialMemory, "", None)
}

object AutomataBuilder {
  val emptyTransitions = Map.empty[String, List[TransitionDestination[Any]]]

  implicit class AnyAutomataBuilder(val automataBuilder: AutomataBuilder[Unit]) extends AnyVal {
    def stateless(initialState: String): LexerAutomata[Unit] = automataBuilder.start[Unit](initialState, ())
  }
}
