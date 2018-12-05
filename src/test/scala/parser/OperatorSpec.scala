package parser

import org.scalatest.{FlatSpec, Matchers}
import ExpressionUtils._

class OperatorSpec extends FlatSpec with Matchers {
  "Operator" should "replace left most when #replaceLeftMost is called on multilevel AST" in {
    val result = ast((1, '+', 2), '+', 3).replaceLeftMost(4)
    result._1 shouldBe ast((4, '+', 2), '+', 3)
    result._2 shouldBe Number(1)
  }

  it should "replace left most leaf when #replaceLeftMost is called on single level AST" in {
    val result = ast(1, '+', 2).replaceLeftMost(3)
    result._1 shouldBe ast(3, '+', 2)
    result._2 shouldBe Number(1)
  }

  "Operator#collectLeft" should "find single element when leaf node" in {
    val result = ast(1, '+', 2).collectLeft()
    result shouldBe List(ast(1, '+', 2))
  }

  it should "find all left elements when chain of multiple elements" in {
    val result = ast((1, '+', 2), '*', (4, '+', 4)).collectLeft()
    result shouldBe List(ast((1, '+', 2), '*', (4, '+', 4)), ast(1, '+', 2))

  }

  "Operator#remap" should "reverse all nodes from root when mapped each node as reverse" in {
    val input = ast(ast((1, '+', 2), '+', (3, '*', 4)), '/', (5, '+', 6))
    val expected = ast((6, '+', 5), '/', ast((4, '*', 3), '+', (2, '+', 1)))
    input remap { op => Operator(op.right, op.left, op.signature) } shouldBe expected
  }
  "Operator#remap" should "preserve original tree when transformed with identity" in {
    val input = ast(ast((1, '+', 2), '+', (3, '*', 4)), '/', (5, '+', 6))
    input remap { identity } shouldBe theSameInstanceAs (input)
  }
  "Operator#replaceFirstLeft" should "replace first left operator matching filter" in {
    val input = ast(ast((1, '+', 2), '+', (3, '+', 4)), '+', Number(5))
    val res = input.replaceFirstLeft(_.right.isInstanceOf[OperatorLike]){ op => Operator(op.left, Number(42), Operators.plus) }
    res._1 shouldBe ast(ast((1, '+', 2), '+', Number(42)), '+', Number(5))
    res._2 shouldBe ast((1, '+', 2), '+', (3, '+', 4))
  }

}
