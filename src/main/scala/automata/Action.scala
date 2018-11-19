package automata

case class Action[-M](pushToken: Boolean, accumulate: Boolean, nextMemory: M => Any)

object NoAction extends Action[Any](false, false, identity)

object Accumulate extends Action[Any](false, true, identity)
