package automata

case class Action[-M](pushToken: Option[Marker], accumulate: Boolean, nextMemory: M => Any) {
  def +[Z <: M](that: Action[Z]): Action[M] = {
    require(this.pushToken.isEmpty || that.pushToken.isEmpty, "One or both actions should not push token")

    Action(pushToken = this.pushToken.orElse(that.pushToken),
      accumulate = this.accumulate || that.accumulate,
      nextMemory = nextMemory andThen { any => that.nextMemory(any.asInstanceOf[Z]) })
  }
}

object Action {
  def mutateMemory[M](nextMemory: M => Any): Action[M] = Action(None, false, nextMemory)
  def pushToken[M](marker: Marker): Action[M] = Action[Any](Some(marker), false, identity)
}

object NoAction extends Action[Any](None, false, identity)

object Accumulate extends Action[Any](None, true, identity)

