package automata

sealed abstract class Condition[-M] {
  def accepts(symbol: Symbol, memory: M, accumulator: String): Boolean
  protected val priority: Int = 0

}

object Condition {
  def ordering[M]: Ordering[Condition[M]] = (x: Condition[M], y: Condition[M]) => x.priority - y.priority
}

case class Match[-M](char: Symbol) extends Condition[M] {
  override def accepts(char: Symbol, memory: M, accumulator: String): Boolean = this.char == char
}


object Else extends Condition[Any] {
  override def accepts(char: Symbol, memory: Any, accumulator: String): Boolean = true
  override protected val priority: Int = 1
}

