package automata

object MultiMap {
  implicit class ListMultiMap[K, V](val map: Map[K, List[V]]) extends AnyVal {
    def addBinding[B >: V](key: K, value: B): Map[K, List[B]] =
      map + (key -> { value :: map.getOrElse(key, Nil) })
  }
}
