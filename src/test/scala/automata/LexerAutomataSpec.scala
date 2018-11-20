package automata

import org.scalatest.{FlatSpec, Matchers}

import scalaz.Alpha.{A, B, C}

class LexerAutomataSpec extends FlatSpec with Matchers {
  "Stateless Automata" should "have starting state" in {
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

  it should "accumulate if requested" in {
    val automata = LexerAutomata.translate[Unit]("start", "end", Accumulate)
      .stateless("start")

    val next = automata << Symbol.Char(A)

    next.isInstanceOf[RunningAutomata[Unit]] shouldBe true
    next.asInstanceOf[RunningAutomata[Unit]].accumulator shouldBe "a"
  }

  "Stateful Automata" should "do transition given memory check passes" in {
    val automata = LexerAutomata.translate[Boolean]("start", "condPassed", ExprCondition[Boolean]((_, m, _) => m))
      .translate("start", "notPassed")
      .start("start", true)

    val next = automata << Symbol.Char(A)

    next.state shouldBe "condPassed"
  }

  it should "skip transition given memory check failes" in {
    val automata = LexerAutomata.translate[Boolean]("start", "condPassed", ExprCondition[Boolean]((_, m, _) => m))
      .translate("start", "notPassed")
      .start("start", false)

    val next = automata << Symbol.Char(A)

    next.state shouldBe "notPassed"
  }

  it should "change memory, push token when requsted and matching char" in {
    val automata = LexerAutomata.translate[Boolean]("start", "passed", Match(Symbol.Char(A)), Action(true, true, { b: Boolean => !b }))
      .start("start", true)
    val next = automata << Symbol.Char(A)

    next.state shouldBe "passed"
    next shouldBe a [RunningAutomata[_]]

    val nextRunning = next.asInstanceOf[RunningAutomata[_]]

    nextRunning.memory shouldBe false
    nextRunning.accumulator shouldBe ""
    nextRunning.token shouldBe Some("a")
  }

  it should "accumulate without pushing token when requested" in {
    val automata = LexerAutomata.translate[Unit]("start", "passed", Match(Symbol.Char(A)), Action(false, true, identity[Unit]))
        .stateless("start")

    val next = automata << Symbol.Char(A)

    next shouldBe a [RunningAutomata[_]]
    val nextRunning = next.asInstanceOf[RunningAutomata[_]]

    nextRunning.accumulator shouldBe "a"
    nextRunning.token shouldBe None
  }

  it should "accumuluate several symbols" in {
    val automata = LexerAutomata.translate[Unit]("start", "start", Action(false, true, identity[Unit]))
      .stateless("start")

    val next = automata << Symbol.Char(A) << Symbol.Char(B)

    next shouldBe a [RunningAutomata[_]]
    val nextRunning = next.asInstanceOf[RunningAutomata[_]]

    nextRunning.accumulator shouldBe "ab"
    nextRunning.token shouldBe None
  }

  it should "reset token when in pushed token state action" in {
    val automata = LexerAutomata.translate[Unit]("start", "accumulate", Action(false, true, identity[Unit]))
        .translate("accumulate", "token", Action(true, false, identity[Unit]))
        .translate("token", "reset", Action(false, false, identity[Unit]))
        .stateless("start")

    val step1 = automata << Symbol.Char(A) << Symbol.Char(B)
    step1.asInstanceOf[RunningAutomata[_]].token shouldBe Some("a")

    val step2 = automata << Symbol.Char(C)
    step2.asInstanceOf[RunningAutomata[_]].token shouldBe None
  }

  "FailedAutomata" should "fail to accept new symbols" in {
    val automata = FailedAutomata("error", "message")

    assertThrows[IllegalStateException] {
      automata << Symbol.None
    }
  }
}
