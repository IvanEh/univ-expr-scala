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

}
