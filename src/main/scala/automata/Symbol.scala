package automata

abstract sealed class Symbol

object Symbol {
  object None extends Symbol
  object Terminal extends Symbol
  case class Char(char: scala.Char) extends Symbol

  implicit def char2CharSymbol(char: scala.Char): Symbol = if (char == '\0') Symbol.Terminal else Symbol.Char(char)
}



