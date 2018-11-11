package automata

import org.scalatest.{FlatSpec, Matchers}

class LexerAutomataSpec extends FlatSpec with Matchers {
  "Automata" should "have starting state" in {
    val automata = LexerAutomata.start("start")

    automata.state shouldBe "start"
  }
  it should "transition to next state conditionless" in {
    val automata = LexerAutomata.translate("start", "next").start("start")

    val next = automata.accept(Symbol.None)

    next.state shouldBe "next"
  }
  it should "transition to error state" in {
    val automata = LexerAutomata.translate("start", "error")
      .describeError("error", "error message")
      .start("start")

    val next = automata.accept(Symbol.None)

    next.state shouldBe "error"
    next.isError shouldBe true
    next.isInstanceOf[FailedLexerAutomata] shouldBe true
    next.asInstanceOf[FailedLexerAutomata].error shouldBe "error message"
  }
}
