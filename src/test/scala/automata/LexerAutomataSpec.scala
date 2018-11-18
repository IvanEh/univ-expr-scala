package automata

import org.scalatest.{FlatSpec, Matchers}

import scalaz.Alpha.{A, B}

class LexerAutomataSpec extends FlatSpec with Matchers {
  "Automata" should "have starting state" in {
    val automata = LexerAutomata.start("start")

    automata.state shouldBe "start"
  }

  it should "transition to next state conditionless" in {
    val automata = LexerAutomata.translate[Unit]("start", "next").stateless("start")

    val next = automata << Symbol.None

    next.state shouldBe "next"
  }

  it should "transition to error state" in {
    val automata = LexerAutomata.translate[Unit]("start", "error")
      .describeError("error", "error message")
      .stateless("start")

    val next = automata << Symbol.None

    next.state shouldBe "error"
    next.isFailed shouldBe true
    next.error shouldBe Some("error message")
    next.isInstanceOf[FailedAutomata[_]] shouldBe true
  }
  it should "transition given matching charachter" in {
    val automata = LexerAutomata.translate("start", "A", Match(Symbol.Char(A)))
      .translate("start", "B", Match(Symbol.Char(B)))
      .translate("start", "end")
      .stateless("start")

    val next = automata << Symbol.Char(B)

    next.state shouldBe "B"
  }

  "FailedAutomata" should "fail to accept new symbols" in {
    val automata = FailedAutomata("error", "message")

    assertThrows[IllegalStateException] {
      automata << Symbol.None
    }
  }
}
