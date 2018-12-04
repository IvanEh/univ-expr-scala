package parser

import automata.Action._
import automata.Condition.{whenMemory, _}
import automata._
import com.typesafe.scalalogging.StrictLogging

import scalaz.{\/, \/-}

class SimpleExpressionParser(expression: String, unaryFunctions: List[String] = Nil) extends StrictLogging {
  private var tokensOrTree: Option[List[Token]] = None

  def doLexing(): LexerError \/ List[Token] = {
    val result = Lexer.parse(expression, automata)

    logger.debug(s"Result of parsing $expression is $result")
    updateStateWithTokenList(result)
    result
  }


  def doSemAnalysis(): Option[SemanticError] = {
    require(tokensOrTree.isDefined, "Lexing was not performed before semantic analysis")

    val invalidFunction = tokensOrTree.get
      .filter(_.marker == TokenType.Function)
      .filterNot(unaryFunctions contains _.value.toLowerCase)
      .headOption

    invalidFunction.map(SemanticError(_, "Function doesn't exist"))
  }

  def doTokenParsing() = {

  }

  private def updateStateWithTokenList(result: \/[LexerError, List[Token]]) = {
    result match {
      case \/-(tokens) => tokensOrTree = Some(tokens)
      case _ =>
    }
  }

  import parser.ExpressionMemory._
  import parser.TokenType._

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

    .translate("pushOpenBracket", "readExpression", Match(Symbol.None), pushOpenBracket + increaseBrackets + setBoundaryOperand)

    .translate("pushCloseBracket", "waitForOperator", Match(Symbol.None) && whenPositiveBrackets, pushCloseBracket + decreaseBrackets)
    .translate("pushCloseBracket", "errorUnbalanced", Match(Symbol.None) && whenBalancedBrackets)

    .translate("readNumber", "readNumber", whenDigit, Accumulate)
    .translate("readNumber", "readDecimal", Match('.'), Accumulate)
    .translate("readNumber", "pushOperator", whenOperator, Accumulate + pushNumber) // Mutual 1
    .translate("readNumber", "waitForOperator", whenSpace, pushNumber)   // Half-Mutual
    .translate("readNumber", "pushCloseBracket", whenCloseBracket && whenPositiveBrackets, pushNumber + Accumulate) // Mutual 2
    .translate("readNumber", "success", whenFinishing, pushNumber) // Mutual 3
    .translate("readNumber", "errorCharAtNumber", whenAlpha)
    .translate("readNumber", "errorInvalidChar")

    .translate("readDecimal", "readDecimal", whenDigit, Accumulate)
    .translate("readDecimal", "pushOperator", whenOperator, Accumulate + pushDecimal) // Mutual 1
    .translate("readDecimal", "waitForOperator", whenSpace, pushDecimal) // Half-Mutual
    .translate("readDecimal", "pushCloseBracket", whenCloseBracket && whenPositiveBrackets, pushDecimal + Accumulate) // Mutual 2
    .translate("readDecimal", "success", whenFinishing, pushDecimal) // Mutual 3
    .translate("readDecimal", "errorDecimalPoint", Match('.'))
    .translate("readDecimal", "errorCharAtNumber", whenAlpha)
    .translate("readDecimal", "errorInvalidChar")

    .translate("readVarOrFunc", "readVarOrFunc", whenDigit || whenAlpha, Accumulate)
    .translate("readVarOrFunc", "waitForFuncOrOperator", whenSpace, pushVariable)
    .translate("readVarOrFunc", "pushOperator", whenOperator, Accumulate + pushVariable) // Mutual 1
    .translate("readVarOrFunc", "pushOpenBracket", whenOpenBracket, pushFunction + Accumulate)
    .translate("readVarOrFunc", "pushCloseBracket", whenCloseBracket && whenPositiveBrackets, pushVariable + Accumulate) // Mutual 2
    .translate("readVarOrFunc", "success", whenFinishing, pushVariable) // Mutual 3

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
    .translate("pushOperator", "readExpression", Match(Symbol.None), pushOperator + setInnerOperand)

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

object TokenType extends Enumeration {
  val Number, Variable, Function, Decimal, OpenBracket, CloseBracket, Operator = Val()
  val pushNumber = pushToken(Number)
  val pushVariable = pushToken(Variable)
  val pushFunction = pushToken(Function)
  val pushDecimal = pushToken(Decimal)
  val pushOpenBracket = pushToken(OpenBracket)
  val pushCloseBracket = pushToken(CloseBracket)
  val pushOperator = pushToken(Operator)

  protected case class Val() extends super.Val with Marker
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

case class SemanticError(token: Token, description: String) {
  override def toString: String = s"Semantic error. Token $token: $description"
}