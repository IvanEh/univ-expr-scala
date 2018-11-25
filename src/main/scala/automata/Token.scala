package automata

case class Token(value: String, marker: Marker) {
  override def toString: String = (s"'$value'", marker).toString()
}
