package automata

private[automata] case class TransitionDestination[-M](to: String, action: Action, condition: Condition[M])

private[automata] object TransitionDestination {
  implicit def tuple3TransitionDestination[M](tuple: (Condition[M], (String, Action))): TransitionDestination[M]
    = TransitionDestination(tuple._2._1, tuple._2._2, tuple._1)
}

