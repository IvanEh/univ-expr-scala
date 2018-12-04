package parser

trait Expression

trait Atom extends Expression

trait NumberLike extends Atom

case class Number(value: Int) extends NumberLike {
  override def toString: String = value.toString
}

case class Decimal(value: Double) extends NumberLike {
  override def toString: String = value.toString
}

case class Variable(name: String) extends Atom {
  override def toString: String = name.toString
}

case class Operator(left: Expression, right: Expression, signature: OperatorSignature) extends Expression {

  def replaceLeftMost(node: Expression): (Operator, Expression) = {
    if (!left.isInstanceOf[Operator]) {
      val newNode = Operator(node, right, signature)
      (newNode, left)
    } else {
      val result = left.asInstanceOf[Operator].replaceLeftMost(node)
      (Operator(result._1, right, signature), result._2)
    }
  }

  def nonLeafOperator(): Boolean = left.isInstanceOf[Operator] || right.isInstanceOf[Operator]
  def leafOperator(): Boolean = !nonLeafOperator()

  override def toString: String = s"($left${signature.sign}$right)"
}

case class OperatorSignature(sign: Char, precedence: Int, commutative: Boolean = false)

case class Function(operand: Expression, name: String) extends Expression {
  override def toString: String = s"$name($operand)"
}

object Operators {
  val multiplication: OperatorSignature = OperatorSignature('*', 2, commutative = true)
  val division: OperatorSignature = OperatorSignature('/', 2)
  val minus: OperatorSignature = OperatorSignature('-', 1)
  val plus: OperatorSignature = OperatorSignature('+', 1, commutative = true)

  def lookup(sign: Char): OperatorSignature = sign match {
    case '*' => multiplication
    case '/' => division
    case '-' => minus
    case '+' => plus
  }
}

object ExpressionUtils {
  implicit def int2Expression(number: Int): Expression = Number(number)
  def ast(l: Expression, op: Char, r: Expression): Operator = Operator(l, r, Operators.lookup(op))
  implicit def exprTuple2Operator(t: (Expression, Char, Expression)): Operator = ast(t._1, t._2, t._3)
  implicit def intTuple2Operator(t: (Int, Char, Int)): Operator = ast(t._1, t._2, t._3)

}
