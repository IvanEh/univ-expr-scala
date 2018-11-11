package automata

abstract sealed class Symbol

object Symbol {
  object None extends Symbol
  object Terminal extends Symbol
  case class Char(char: scala.Char) extends Symbol
}



