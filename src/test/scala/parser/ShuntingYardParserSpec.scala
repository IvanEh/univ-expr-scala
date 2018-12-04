package parser

import automata.Token
import org.scalatest.{FlatSpec, Matchers}

class ShuntingYardParserSpec extends FlatSpec with Matchers {
  "Shunting Yard Parser" should "fail on empty input" in {
    assertThrows[IllegalStateException] {
      ShuntingYardParser.parse(Nil)
    }
  }
  it should "parse literal expression" in {
    val result = ShuntingYardParser.parse(Token("1", TokenType.Number) :: Nil)
    result shouldBe Number(1)
  }
  it should "parse simple tree" in {
    val result = ShuntingYardParser.parse(
      Token("1.0", TokenType.Decimal) ::
      Token("+", TokenType.Operator) ::
      Token("v", TokenType.Variable) :: Nil)

    result shouldBe Operator(Decimal(1.0), Variable("v"), Operators.plus)
  }
  it should "parse expression with functions and nesting with different operands priority" in {
    val result = ShuntingYardParser.parse(
      Token("(", TokenType.OpenBracket) ::
      Token("1", TokenType.Number) ::
      Token("+", TokenType.Operator) ::
      Token("a", TokenType.Variable) ::
      Token(")", TokenType.CloseBracket) ::
      Token("*", TokenType.Operator) ::
      Token("b", TokenType.Variable) ::
      Token("/", TokenType.Operator) ::
      Token("f", TokenType.Function) ::
      Token("c", TokenType.Variable) :: Nil)

    val plus = Operator(Number(1), Variable("a"), Operators.plus)
    val func = Function(Variable("c"), "f")
    val mult = Operator(plus, Variable("b"), Operators.multiplication)
    val div = Operator(mult, func, Operators.division)

    result shouldBe div
  }
}
