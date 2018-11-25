package automata

case class Action[-M](pushToken: Boolean, accumulate: Boolean, nextMemory: M => Any) {
  def +[Z <: M](that: Action[Z]): Action[M] = Action(pushToken = this.pushToken || that.pushToken,
    accumulate = this.accumulate || that.accumulate,
    nextMemory = nextMemory andThen { any => that.nextMemory(any.asInstanceOf[Z]) })
}

object Action {
  def mutateMemory[M](nextMemory: M => Any): Action[M] = Action(false, false, nextMemory)
}

object NoAction extends Action[Any](false, false, identity)

object Accumulate extends Action[Any](false, true, identity)

object PushToken extends Action[Any](true, false, identity)
