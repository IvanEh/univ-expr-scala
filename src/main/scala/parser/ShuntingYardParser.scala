package parser

import automata.Token

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ShuntingYardParser {

  def parse(tokens: List[Token]): Expression = {
    var output = List[Expression]()
    val operatorStack = mutable.ArrayBuffer[Token]()

    for (token <- tokens) {
      output = token match {
        case Token(v, TokenType.Number) => Number(v.toInt) :: output
        case Token(v, TokenType.Decimal) => Decimal(v.toDouble) :: output
        case Token(v, TokenType.Variable) => Variable(v) :: output
        case Token(_, TokenType.Function) => operatorStack += token; output
        case Token(op, TokenType.Operator) => acceptOperator(op, output, operatorStack)
        case Token(_, TokenType.OpenBracket) => operatorStack += token; output
        case Token(_, TokenType.CloseBracket) => acceptCloseBracket(output, operatorStack)
      }
    }

    output = transferRemainingOperators(output, operatorStack)

    output match {
      case result :: Nil => result
      case Nil => throw new IllegalStateException("Invalid input tokens resulted in empty expression")
      case _ => throw new IllegalStateException(s"Invalid input tokens resulted in erroneous output state: $output")
    }
  }

  private def acceptCloseBracket(prevOutput: List[Expression], operatorStack: ArrayBuffer[Token]): List[Expression] = {
    var output = prevOutput
    while (operatorStack.last.marker != TokenType.OpenBracket) {
      output = transferOperator(output, operatorStack.last)
      operatorStack.remove(operatorStack.size - 1)
    }
    operatorStack.remove(operatorStack.size - 1)

    output
  }


  private def acceptOperator(op: String, prevOutput: List[Expression], operatorStack: ArrayBuffer[Token]): List[Expression] = {
    var output = prevOutput
    val precedence = Operators.lookup(op(0)).precedence
    while (operatorStack.nonEmpty && shouldPopOperators(operatorStack, precedence)) {
      output = transferOperator(output, operatorStack.head)
      operatorStack.remove(operatorStack.size - 1)
    }
    operatorStack append Token(op, TokenType.Operator)

    output
  }

  private def transferRemainingOperators(prevOutput: List[Expression], operatorStack: ArrayBuffer[Token]): List[Expression] = {
    var output = prevOutput
    while (operatorStack.nonEmpty) {
      output = transferOperator(output, operatorStack.last)
      operatorStack.remove(operatorStack.size - 1)
    }

    output
  }

  private def transferOperator(prevOutput: List[Expression], token: Token): List[Expression] = token match {
    case Token(funcName, TokenType.Function) => Function(prevOutput.head, funcName) :: prevOutput.tail
    case Token(op, TokenType.Operator) => Operator(prevOutput.tail.head, prevOutput.head, Operators.lookup(op(0))) :: Nil
    case _ => throw new IllegalStateException("Cannot transfer anything except function and operator from operator stack")
  }

  def shouldPopOperators(operatorStack: ArrayBuffer[Token], precedence: Int): Boolean = operatorStack.head match {
    case Token(_, TokenType.Function) => true
    case Token(op, TokenType.Operator) => Operators.lookup(op(0)).precedence >= precedence
    case _ => false
  }

}
