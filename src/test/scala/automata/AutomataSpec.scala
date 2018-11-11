package automata

import org.scalatest.{FlatSpec, Matchers}

class AutomataSpec extends FlatSpec with Matchers {
  "Automata" should "have starting state" in {
    val automata = Automata.start("start")

    automata.state shouldBe "start"
  }
  it should "transition to next state conditionless" in {
    val automata = Automata.translate("start", "next").start("start")

    val next = automata.accept(Symbol.None)

    next.state shouldBe "next"
  }
}
