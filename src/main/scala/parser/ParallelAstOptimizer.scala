package parser

case class ParallelAstOptimizer() {
  val multiplicationOptimizer = ReverseOperationOptimizer(Operators.multiplication, Operators.division)
  val additionOptimizer = ReverseOperationOptimizer(Operators.plus, Operators.minus)
  val computedMarker = ComputedOperatorsMarker()

  def optimize(expr: Expression): Expression = expr match {
    case op: OperatorLike =>
      var optimized = op remap { node => CommutativeOperatorLeftLeaningNormalization(node).visitAndMap() }

      var iteration = 0
      while (!optimized.isInstanceOf[ComputedOperator]) {
        optimized = optimized
          .remap { node => computedMarker.mark(node, iteration) }
          .remap { node => multiplicationOptimizer.optimize(node, iteration) }
          .remap { node => additionOptimizer.optimize(node, iteration)}
        iteration = iteration + 1
      }
      optimized
    case _ => expr
  }
}

abstract class AstVisitingMapper(val target: OperatorLike) {
  protected def accepts(): Boolean
  protected def doMap(): OperatorLike

  final def visitAndMap(): OperatorLike = if (accepts()) {
    doMap()
  } else {
    target
  }
}

case class CommutativeOperatorLeftLeaningNormalization(override val target: OperatorLike) extends AstVisitingMapper(target) {

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

class ComputedOperator(that: OperatorLike, val iteration: Int) extends OperatorLike {
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
        signature == that.signature &&
        iteration == that.iteration
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(left, right, signature, iteration)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

case class ComputedOperatorsMarker() {
  def mark(operator: OperatorLike, iteration: Int): OperatorLike = {
    if (isComputedPreviously(operator.left, iteration) && isComputedPreviously(operator.right, iteration))
      new ComputedOperator(operator, iteration)
    else
      operator
  }

  private def isComputedPreviously(operator: Expression, iterator: Int) = operator match {
    case c: ComputedOperator => iterator > c.iteration
    case _: OperatorLike => false
    case _ => true
  }
}

case class ReverseOperationOptimizer(commutative: OperatorSignature, reverse: OperatorSignature) {

  def optimize(op: OperatorLike, iteration: Int): OperatorLike = {
    if (matchingOperator(op) && isComputedPreviously(op.right, iteration) && isLeftLeaning(op)
      && !isComputed(op.left.asInstanceOf[OperatorLike])
      && !isComputed(op))
      doOptimize(op, iteration)
    else
      op
  }

  private def isComputed(op: OperatorLike): Boolean = op match {
    case _: ComputedOperator => true
    case _ => false
  }


  private def isLeftLeaning(op: OperatorLike) = {
    op.left.isInstanceOf[OperatorLike]
  }

  private def matchingOperator(op: OperatorLike) = {
    (commutative, reverse).productIterator contains op.signature
  }

  protected def doOptimize(parent: OperatorLike, iteration: Int): OperatorLike = {
    val (newChild: OperatorLike, replaced) = parent.left.asInstanceOf[OperatorLike]
      .replaceFirstLeft(child => anyComputed(child, iteration) || !matchingOperator(child)) { child =>

      if (!matchingOperator(child)) // Switch of operator happened
        child
      else
        if(isComputedPreviously(child.left, iteration))
          new ComputedOperator(Operator(child.left, parent.right, parent.signature), iteration)
        else if (isComputedPreviously(child.right, iteration))
          (parent.signature.commutative, child.signature.commutative) match {
            case (true, true) =>
              val computed = new ComputedOperator(Operator(child.right, parent.right, parent.signature), iteration)
              Operator(child.left, computed, commutative)
            case (true, false) =>
              val computed = new ComputedOperator(Operator(parent.right, child.right, reverse), iteration)
              Operator(child.left, computed, commutative)
            case (false, true) =>
              val computed = new ComputedOperator(Operator(child.right, parent.right, reverse), iteration)
              Operator(child.left, computed, commutative)
            case (false, false) =>
              val computed = new ComputedOperator(Operator(child.right, parent.right, commutative), iteration)
              Operator(child.left, computed, reverse)
          }
        else
          child
    }

    if (matchingOperator(replaced)) {
      if (isComputedPreviously(replaced.left, iteration))
        Operator(newChild, replaced.right, replaced.signature)
      else
        newChild
    } else {
      parent
    }

  }

  private def anyComputed(child: OperatorLike, iteration: Int) = {
    isComputedPreviously(child.right, iteration) || isComputedPreviously(child.left, iteration)
  }

  def isSameOperation(high: OperatorLike, low: OperatorLike): Boolean = high.signature == low.signature

  private def isComputedPreviously(expr: Expression, iteration: Int): Boolean = expr match {
    case op: ComputedOperator => iteration > op.iteration
    case _: OperatorLike => false
    case _ => true
  }
}