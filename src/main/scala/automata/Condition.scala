package automata

sealed abstract class Condition[-M] {
  def accepts(symbol: Symbol, memory: M, accumulator: String): Boolean
  protected val priority: Int = 0

  def &&[Z <: M](that: Condition[Z]): Condition[Z]
    = ExprCondition({ (symb, mem, acc) => this.accepts(symb, mem, acc) && that.accepts(symb, mem, acc)})
  def ||[Z <: M](that: Condition[Z]): Condition[Z]
  = ExprCondition({ (symb, mem, acc) => this.accepts(symb, mem, acc) || that.accepts(symb, mem, acc)})
  def neg(): Condition[M] = ExprCondition({ (symb, mem, acc) => !this.accepts(symb, mem, acc)})
}

object Condition {
  def ordering[M]: Ordering[Condition[M]] = (x: Condition[M], y: Condition[M]) => x.priority - y.priority
  def anyChar(chars: Char*): Condition[Any] = CharCondition({ chars contains _})
  def whenMemory[M](cond: M => Boolean): Condition[M] = ExprCondition({ (_, mem, _) => cond(mem) })
  def whenAlpha: Condition[Any] = CharCondition({ _.isLetter })
  val whenTerminal: Condition[Any] = Match(Symbol.Terminal)
  val whenDigit: Condition[Any] = CharCondition({ _.isDigit })

}

case class Match[-M](char: Symbol) extends Condition[M] {
  override def accepts(char: Symbol, memory: M, accumulator: String): Boolean = this.char == char
}

case class ExprCondition[M](expr: (Symbol, M, String) => Boolean) extends Condition[M] {
  override def accepts(symbol: Symbol, memory: M, accumulator: String): Boolean = expr(symbol, memory, accumulator)
}

case class CharCondition(expr: Char => Boolean) extends Condition[Any] {
  override def accepts(symbol: Symbol, memory: Any, accumulator: String): Boolean = symbol match {
    case Symbol.Char(c) => expr(c)
    case _ => false
  }
}

private[automata] object Else extends Condition[Any] {
  override def accepts(char: Symbol, memory: Any, accumulator: String): Boolean = char != Symbol.None
  override protected val priority: Int = 1
}

