package parser

trait Expression

trait Atom extends Expression

trait NumberLike extends Atom

case class Number(value: Int) extends NumberLike

case class Decimal(value: Double) extends NumberLike

case class Variable(name: String) extends Atom

case class Operator(left: Expression, right: Expression, signature: OperatorSignature) extends Expression

case class OperatorSignature(sign: Char, precedence: Int)

case class Function(operand: Expression, name: String) extends Expression

object Operators {
  val multiplication: OperatorSignature = OperatorSignature('*', 2)
  val division: OperatorSignature = OperatorSignature('/', 2)
  val minus: OperatorSignature = OperatorSignature('-', 1)
  val plus: OperatorSignature = OperatorSignature('+', 1)

  def lookup(sign: Char): OperatorSignature = sign match {
    case '*' => multiplication
    case '/' => division
    case '-' => minus
    case '+' => plus
  }
}
