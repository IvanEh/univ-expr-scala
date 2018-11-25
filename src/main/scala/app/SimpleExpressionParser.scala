package app

import automata.Action.mutateMemory
import automata.Condition.{whenMemory, _}
import automata._
import com.typesafe.scalalogging.StrictLogging

import scalaz.\/

class SimpleExpressionParser(expression: String) extends StrictLogging {
  def doLexing(): LexerError \/ List[String] = {
    val result = Lexer.parse(expression, automata)

    logger.debug(s"Result of parsing $expression is $result")
    result
  }

  import app.ExpressionMemory._

  private val whenOperator = anyChar('-', '+', '*', '/')
  private val whenSpace = Match(' ')
  private val whenOpenBracket = Match('(')
  private val whenCloseBracket = Match(')')
  private val whenFinishing = whenTerminal && whenBalancedBrackets

  private val automata = LexerAutomata
    .translate("readExpression", "readExpression", whenSpace)
    .translate("readExpression", "readNumber", whenDigit, Accumulate)
    .translate("readExpression", "readVarOrFunc", whenAlpha, Accumulate)
    .translate("readExpression", "pushOpenBracket", whenOpenBracket, Accumulate)
    .translate("readExpression", "errorOperatorAtExpression", whenOperator)
    .translate("readExpression", "errorExpectedSecondOperand", whenFinishing) // test
    .translate("readExpression", "errorBoundaryClosingBracket", whenCloseBracket && whenBoundaryOperand)
    .translate("readExpression", "errorUnbalanced", whenBalancedBrackets && whenOuterOperand && whenCloseBracket, Accumulate)
    .translate("readExpression", "errorInvalidChar")

    .translate("pushOpenBracket", "readExpression", Match(Symbol.None), PushToken + increaseBrackets + setBoundaryOperand)

    .translate("pushCloseBracket", "waitForOperator", Match(Symbol.None) && whenPositiveBrackets, PushToken + decreaseBrackets)
    .translate("pushCloseBracket", "errorUnbalanced", Match(Symbol.None) && whenBalancedBrackets)

    .translate("readNumber", "readNumber", whenDigit, Accumulate)
    .translate("readNumber", "readDecimal", Match('.'), Accumulate)
    .translate("readNumber", "pushOperator", whenOperator, Accumulate + PushToken) // Mutual 1
    .translate("readNumber", "waitForOperator", whenSpace, PushToken)   // Half-Mutual
    .translate("readNumber", "pushCloseBracket", whenCloseBracket && whenPositiveBrackets, PushToken + Accumulate) // Mutual 2
    .translate("readNumber", "success", whenFinishing, PushToken) // Mutual 3
    .translate("readNumber", "errorCharAtNumber", whenAlpha)
    .translate("readNumber", "errorInvalidChar")

    .translate("readDecimal", "readDecimal", whenDigit, Accumulate)
    .translate("readDecimal", "pushOperator", whenOperator, Accumulate + PushToken) // Mutual 1
    .translate("readDecimal", "waitForOperator", whenSpace, PushToken) // Half-Mutual
    .translate("readDecimal", "pushCloseBracket", whenCloseBracket && whenPositiveBrackets, PushToken + Accumulate) // Mutual 2
    .translate("readDecimal", "success", whenFinishing, PushToken) // Mutual 3
    .translate("readDecimal", "errorDecimalPoint", Match('.'))
    .translate("readDecimal", "errorCharAtNumber", whenAlpha)
    .translate("readDecimal", "errorInvalidChar")

    .translate("readVarOrFunc", "readVarOrFunc", whenDigit || whenAlpha, Accumulate)
    .translate("readVarOrFunc", "waitForFuncOrOperator", whenSpace, PushToken)
    .translate("readVarOrFunc", "pushOperator", whenOperator, Accumulate + PushToken) // Mutual 1
    .translate("readVarOrFunc", "pushOpenBracket", whenOpenBracket, PushToken + Accumulate)
    .translate("readVarOrFunc", "pushCloseBracket", whenCloseBracket && whenPositiveBrackets, PushToken + Accumulate) // Mutual 2
    .translate("readVarOrFunc", "success", whenFinishing, PushToken) // Mutual 3

    .translate("waitForFuncOrOperator", "pushOperator", whenOperator, Accumulate)
    .translate("waitForFuncOrOperator", "waitForFuncOrOperator", whenSpace)
    .translate("waitForFuncOrOperator", "pushOpenBracket", whenOpenBracket, Accumulate)
    .translate("waitForFuncOrOperator", "pushCloseBracket", whenCloseBracket, Accumulate)
    .translate("waitForFuncOrOperator", "success", whenFinishing)
    .translate("waitForFuncOrOperator", "pushOpenBracket", whenOpenBracket, Accumulate)
    .translate("waitForFuncOrOperator", "errorExpectedFuncOrOperator")

    .translate("waitForOperator", "pushOperator", whenOperator, Accumulate)
    .translate("waitForOperator", "waitForOperator", whenSpace)
    .translate("waitForOperator", "pushCloseBracket", whenCloseBracket, Accumulate)
    .translate("waitForOperator", "success", whenFinishing)
    .translate("waitForOperator", "errorOperatorExpected")
    .translate("pushOperator", "readExpression", Match(Symbol.None), PushToken + setInnerOperand)

    .describeError("errorDecimalPoint", "Two decimal points")
    .describeError("errorOperatorAtExpression", "Expected number or variable but got operator")
    .describeError("errorInvalidChar", "Invalid Char")
    .describeError("errorExpectedSecondOperand", "Second operand expected")
    .describeError("errorCharAtNumber", "Number ended with char")
    .describeError("errorMissingSecondOperand", "Missing second operand")
    .describeError("errorOperatorExpected", "Operator expected")
    .describeError("errorBoundaryClosingBracket", "Cannot immediatly close expression")
    .describeError("errorUnbalanced", "Brackets are not balanced")
    .describeError("errorExpectedFuncOrOperator", "Expected function or operator")
    .start("readExpression", ExpressionMemory())

}



private case class ExpressionMemory(boundaryOperand: Boolean = true, openBrackets: Int = 0) {
  def setInnerOperand(): ExpressionMemory = ExpressionMemory(boundaryOperand = false, openBrackets)
  def setBoundaryOperand(): ExpressionMemory = ExpressionMemory(boundaryOperand = true, openBrackets)
  def increaseBrackets(): ExpressionMemory = ExpressionMemory(boundaryOperand, openBrackets + 1)
  def decreaseBrackets(): ExpressionMemory = ExpressionMemory(boundaryOperand, openBrackets - 1)
}

object ValueType extends Enumeration {
  val Number, Variable, Function, Decimal = Value
}

private object ExpressionMemory {
  val setInnerOperand: Action[ExpressionMemory] = mutateMemory({_.setInnerOperand()})
  val setBoundaryOperand: Action[ExpressionMemory] = mutateMemory({_.setBoundaryOperand()})
  val increaseBrackets: Action[ExpressionMemory] = mutateMemory({_.increaseBrackets()})
  val decreaseBrackets: Action[ExpressionMemory] = mutateMemory({_.decreaseBrackets()})
  val whenBoundaryOperand: Condition[ExpressionMemory] = whenMemory({ _.boundaryOperand })
  val whenOuterOperand: Condition[ExpressionMemory] = whenMemory({ !_.boundaryOperand })
  val whenBalancedBrackets: Condition[ExpressionMemory] = whenMemory({ _.openBrackets == 0 })
  val whenPositiveBrackets: Condition[ExpressionMemory] = whenMemory({ _.openBrackets > 0 })
}
