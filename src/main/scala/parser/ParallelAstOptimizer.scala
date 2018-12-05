package parser

class ParallelAstOptimizer {

}

abstract class AstVisitingMapper(val target: Expression) {
  protected def accepts(): Boolean
  protected def doMap(): Expression

  final def visitAndMap(): Expression = if (accepts()) {
    doMap()
  } else {
    target
  }
}

case class CommutativeOperatorLeftLeaningNormalization(override val target: Operator) extends AstVisitingMapper(target) {

  private val signature = target.signature

  override def accepts(): Boolean = {
    signature.commutative && matchesTarget(target.right)
  }

  override def doMap(): Operator = {
      val result = target.replaceLeftMost(target.right)
      Operator(result._1.left, result._2, signature)
  }

  private def matchesTarget(op: Expression): Boolean = op match {
    case Operator(_, _, OperatorSignature(sign, _, _)) => sign == signature.sign
    case _ => false
  }
}

class ComputedOperator(that: OperatorLike) extends OperatorLike {
  override val left: Expression = that.left
  override val right: Expression = that.right
  override val signature: OperatorSignature = that.signature

  override def toString = s"[${super.toString}]"


  def canEqual(other: Any): Boolean = other.isInstanceOf[ComputedOperator]

  override def equals(other: Any): Boolean = other match {
    case that: ComputedOperator =>
      (that canEqual this) &&
        left == that.left &&
        right == that.right &&
        signature == that.signature
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(left, right, signature)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

case class ReverseOperationOptimizer(commutative: OperatorSignature, reverse: OperatorSignature) {


  def optimize(op: OperatorLike): OperatorLike = {
    if (matchingOperator(op) && isComputed(op.right) && op.left.isInstanceOf[OperatorLike])
      doOptimize(op)
    else
      op
  }


  private def matchingOperator(op: OperatorLike) = {
    (commutative, reverse).productIterator contains op.signature
  }

  protected def doOptimize(parent: OperatorLike): OperatorLike = {
    val (newChild: OperatorLike, replaced) = parent.left.asInstanceOf[OperatorLike]
      .replaceFirstLeft(child => isComputed(child.right) || isComputed(child.left) || !matchingOperator(child)) { child =>

      if (!matchingOperator(child)) // Switch of operator happened
        child
      else
        if(isComputed(child.left))
          new ComputedOperator(Operator(child.left, parent.right, child.signature))
        else if (isComputed(child.right))
          (parent.signature.commutative, child.signature.commutative) match {
            case (true, true) =>
              val computed = new ComputedOperator(Operator(child.right, parent.right, parent.signature))
              Operator(child.left, computed, commutative)
            case (true, false) =>
              val computed = new ComputedOperator(Operator(parent.right, child.right, reverse))
              Operator(child.left, computed, commutative)
            case (false, true) =>
              val computed = new ComputedOperator(Operator(child.right, parent.right, reverse))
              Operator(child.left, computed, commutative)
            case (false, false) =>
              val computed = new ComputedOperator(Operator(child.right, parent.right, commutative))
              Operator(child.left, computed, reverse)
          }
        else
          child
    }

    if (matchingOperator(replaced)) {
      if (isComputed(replaced.left))
        Operator(newChild, replaced.right, replaced.signature)
      else
        newChild
    } else {
      parent
    }

  }

  def isSameOperation(high: OperatorLike, low: OperatorLike): Boolean = high.signature == low.signature

  private def isComputed(expr: Expression) = expr match {
    case _: ComputedOperator => true
    case _: OperatorLike => false
    case _ => true
  }
}