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
}
