package automata

import app.SimpleExpressionParser
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FlatSpec, Matchers}

import scalaz.\/-

class SimpleExpressionParserSpec extends FlatSpec with Matchers {
  val parseOk = Table(
    ("expression", "tokenCount"),
    ("1",                      1),
    ("1+2",                    3),
    ("1+2+3",                  5),
    ("(1+2+3)",                7),
    ("(1 + 2)",                5),
    ("(1 + 2) + 3",            7),
    (" ( 1 + 2 )+3+((4*4) )",  15),
    ("((1 + 2) + 3)",           9)
  )

  val parseNok = Table(
    ("expression"),
    ("("),
    ("(1"),
    ("( 1"),
    ("( 1 + 2"),
    ("(1 + 2 "),
    (")"),
    ("(1 + 2 +)"),
    ("(1 + 2 + )"),
    ("(1 + 2 + 3) * 4)"),
    ("((1 + 2)+5))")
  )

  forAll(parseOk) { (expression, tokenCount) =>
    "SimpleExpressionParser" should s"parse correclty $expression" in {

      val result = new SimpleExpressionParser(expression).doLexing()
      result match {
        case \/-(tokens) => tokens.size shouldBe tokenCount
        case _ => fail()
      }

    }
  }

  forAll(parseNok) { (expression: String) =>
    "SimpleExpressionParser" should s"yield error for $expression" in {

      val result = new SimpleExpressionParser(expression).doLexing()
      result match {
        case \/-(_) => fail("expected error but got succesful parsing")
        case _ => succeed
      }

    }
  }

}
