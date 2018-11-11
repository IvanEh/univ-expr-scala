package automata

import org.scalatest.{FlatSpec, Matchers}

class AutomataSpec extends FlatSpec with Matchers {
  "Automata" should "have starting state" in {
    val automata = Automata.start("start")

    automata.state shouldBe "start"
  }
}
