package automata

private[automata] case class TransitionDestination[-M](to: String, condition: Condition[M])

private[automata] object TransitionDestination {
  implicit def tuple2TransitionDestination[M](tuple: (String, Condition[M])): TransitionDestination[M]
    = TransitionDestination(tuple._1, tuple._2)
}

