package parser

class ParallelAstOptimizer {

}

case class CommutativeOperatorLeftLeaningNormalization(target: Operator) {

  private val signature = target.signature

  def accepts(): Boolean = {
    signature.commutative && matchesTarget(target.right)
  }

  def normalized(): Operator = if (accepts())
    doNormalization()
  else
    target

  def doNormalization(): Operator = {
      val result = target.replaceLeftMost(target.right)
      Operator(result._1.left, result._2, signature)
  }

  private def mergeLeft() = {
    val targetRight = target.right.asInstanceOf[Operator]
    val targetLeft = target.left.asInstanceOf[Operator]
    val newLeft = Operator(targetLeft, targetRight.left, signature)
    Operator(CommutativeOperatorLeftLeaningNormalization(newLeft).normalized(), targetRight.right, signature)
  }

  private def rotateLeft() = {
    val targetRight = target.right.asInstanceOf[Operator]
    Operator(Operator(target.left, targetRight.right, signature), targetRight.left, signature)
  }

  private def matchesTarget(op: Expression): Boolean = op match {
    case Operator(_, _, OperatorSignature(sign, _, _)) => sign == signature.sign
    case _ => false
  }
}
