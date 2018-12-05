package parser

import org.scalatest.{FlatSpec, Matchers}
import parser.ExpressionUtils._

class ReverseOperationOptimizerSpec extends FlatSpec with Matchers {
  "ReverseOperationOptimizer" should "optimize when same operator and leftmost computed" in {
    val input = ast(ast(1, '*', (2, '+', 3)), '*', 4)
    val optimizer = ReverseOperationOptimizer(Operators.multiplication, Operators.division)
    val result = optimizer.optimize(input)

    result shouldBe ast(new ComputedOperator(ast(1, '*', 4)), '*', (2, '+', 3))
  }
  "ReverseOperationOptimizer" should "oprimize double division" in {
    val input = ast(ast((1, '+', 2), '/', 3), '/', 4)
    val optimizer = ReverseOperationOptimizer(Operators.multiplication, Operators.division)
    val result = optimizer.optimize(input)

    result shouldBe ast((1, '+', 2), '/', new ComputedOperator((3, '*', 4)))
  }
  "ReverseOperationOptimizer" should "oprimize division and multiplication" in {
    val input = ast(ast((1, '+', 2), '/', 3), '*', 4)
    val optimizer = ReverseOperationOptimizer(Operators.multiplication, Operators.division)
    val result = optimizer.optimize(input)

    result shouldBe ast((1, '+', 2), '*', new ComputedOperator((4, '/', 3)))
  }
  it should "not group with inapropriate operations" in {
    val delayed1: Expression = ast((1, '*', 1), '+', 2)
    val delayed2: Expression = (3, '+', 4)
    val input = ast(ast(delayed1, '*', delayed2), '*', Number(5))

    val optimizer = ReverseOperationOptimizer(Operators.multiplication, Operators.division)
    val result = optimizer.optimize(input)

    result shouldBe input
  }

}
