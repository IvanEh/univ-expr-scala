package automata

private[automata] case class TransitionDestination[-M](to: String, action: Action, condition: Condition[M])

private[automata] object TransitionDestination {
  implicit def tuple2TransitionDestination[M](tuple: (String, (Action, Condition[M]))): TransitionDestination[M]
    = TransitionDestination(tuple._1, tuple._2._1, tuple._2._2)
}

