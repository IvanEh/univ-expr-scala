package parser

import org.scalatest.{FlatSpec, Matchers}
import ExpressionUtils._

class ParallelAstOptimizerSpec extends FlatSpec with Matchers {

  "ParallelAstOptimizer" should "not optimize given signle operation expression" in {
    val input = ast(1, '+', 2)
    val res = ParallelAstOptimizer().optimize(input)
    res shouldBe new ComputedOperator((1, '+', 2), 0)
  }

  it should "not optimize non optimizable expression and should switch leaning" in {
    val input = ast(1, '+', (1, '+', 1))
    val res = ParallelAstOptimizer().optimize(input)
    res shouldBe new ComputedOperator(ast(new ComputedOperator((1, '+', 1), 0), '+', 1), 1)
  }

  it should "optimize expression with single pair of operators" in {
    val plus1: Expression = (4, '+', 5)
    val plus2: Expression = (6, '+', 7)
    val leftmost: Expression = (1, '*', 2)
    val input = ast(ast(ast(leftmost, '/', 3), '*', plus1), '/', plus2)

    val res = ParallelAstOptimizer().optimize(input)

    val left = new ComputedOperator(ast(new ComputedOperator((1, '*', 2), 0), '/', 3), 1)
    val right = new ComputedOperator(ast(
      new ComputedOperator((4, '+', 5), 0),
      '/',
      new ComputedOperator((6, '+', 7), 0)
    ), 1)
    val expected = new ComputedOperator(ast(left, '*', right), 2)

    res shouldBe expected
  }

  it should "regroup two operations on the same left leaning" in {
    val delayed: Expression = (1, '+', 2)
    val input = ast(ast(ast(ast(delayed, '/', 3), '/', 4), '*', 5), '/', 6)

    val result = ParallelAstOptimizer().optimize(input)

    val right = new ComputedOperator(ast(5, '/', 6), 0)
    val left = new ComputedOperator(ast(new ComputedOperator((1, '+', 2), 0), '/',new ComputedOperator((3, '*', 4), 0)), 1)
    val expected = new ComputedOperator(ast(left, '*', right), 2)

    result shouldBe expected
  }
  it should "regroup two mixed operations on the same left leaning" in {
    val input = ast(ast(ast(ast((1, '+', 2), '-', 3), '+', 4), '*', 5), '/', 6)

    val result = ParallelAstOptimizer().optimize(input)

    val right = new ComputedOperator(ast(5, '/', 6), 0)
    val left = new ComputedOperator(ast(new ComputedOperator((1, '+', 2), 0), '+',new ComputedOperator((4, '-', 3), 0)), 1)
    val expected = new ComputedOperator(ast(left, '*', right), 2)

    result shouldBe expected
  }
  it should "optimize different left leanings" in {
    val rightInput = ast(ast((5, '-', 6), '-', 7), '-', 8)
    val leftInput = ast(ast((1, '/', 2), '/', 3), '/', 4)
    val input = ast(leftInput, '*', rightInput)

    val result = ParallelAstOptimizer().optimize(input)

    val right = new ComputedOperator(
      ast(new ComputedOperator((5, '-', 6), 0), '-', new ComputedOperator((7, '+', 8), 0)),
      1)
    val left = new ComputedOperator(
      ast(new ComputedOperator((1, '/',2), 0), '/', new ComputedOperator((3, '*', 4), 0)),
      1
    )
    val expected = new ComputedOperator((left, '*', right), 2)

    result shouldBe expected
  }
}
