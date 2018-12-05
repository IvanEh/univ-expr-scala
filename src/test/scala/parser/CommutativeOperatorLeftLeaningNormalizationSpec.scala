package parser

import org.scalatest.{FlatSpec, Matchers}
import parser.ExpressionUtils._

class CommutativeOperatorLeftLeaningNormalizationSpec extends FlatSpec with Matchers {
  "CommutativeOperatorLeftLeaningNormalization" should "return the same for leaf operator" in {
    checkNormalization((1, '+', 2), (1, '+', 2))
  }

  it should "make a simple rotation when single level" in {
    checkNormalization(ast(1, '+', ast(2, '+', 3)),ast(ast(2, '+', 3), '+', 1))
  }

  it should "not normalize for non commutative operations" in {
    checkNormalization(ast(1, '/', ast(2, '/', 3)),ast(1, '/', ast(2, '/', 3)))
  }

  it should "make a complex transition when conflicting rotation" in {
    val input = ast((1, '+', 2), '+', ast((3, '+', 4), '+', 5))
    val expected = ast(ast(ast((3, '+', 4), '+', 5), '+', 2), '+', 1)
    checkNormalization(input, expected)
  }

  private def checkNormalization(input: Operator, expected: Operator) = {
    CommutativeOperatorLeftLeaningNormalization(input).visitAndMap() shouldBe expected
  }
}
