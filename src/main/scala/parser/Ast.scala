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

trait OperatorLike extends Expression {
  val left: Expression
  val right: Expression
  val signature: OperatorSignature

  def collectLeft(): List[OperatorLike] = left match {
    case leftOp: Operator => this :: leftOp.collectLeft()
    case _ => this :: Nil
  }

  def remap(mapper: OperatorLike => OperatorLike): OperatorLike = {
    val newLeft = left match {
      case leftOp: Operator => leftOp.remap(mapper)
      case _ => left
    }
    val newRight = right match {
      case rightOp: Operator => rightOp.remap(mapper)
      case _ => right
    }
    if (newLeft == left && newRight == right) {
      mapper(this)
    } else {
      mapper(Operator(newLeft, newRight, signature))
    }
  }

  def replaceFirstLeft(filter: OperatorLike => Boolean)(replacer: OperatorLike => Expression): (Expression, OperatorLike) = {
    if (filter(this)) {
      (replacer(this), this)
    } else {
      val result = left match {
        case leftOp: OperatorLike => leftOp.replaceFirstLeft(filter)(replacer)
        case _ => (left, this)
      }
      
      (Operator(result._1, right, signature), result._2)
    }
  }

  def replaceLeftMost(node: Expression): (OperatorLike, Expression) = {
    if (!left.isInstanceOf[OperatorLike]) {
      val newNode = Operator(node, right, signature)
      (newNode, left)
    } else {
      val result = left.asInstanceOf[OperatorLike].replaceLeftMost(node)
      (Operator(result._1, right, signature), result._2)
    }
  }

  def nonLeafOperator(): Boolean = left.isInstanceOf[OperatorLike] || right.isInstanceOf[OperatorLike]
  def leafOperator(): Boolean = !nonLeafOperator()

  override def toString: String = s"($left${signature.sign}$right)"
}

case class Operator(left: Expression, right: Expression, signature: OperatorSignature) extends OperatorLike {

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
