package symboltable

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2021/05/18 10:46
 */
trait SortedSymbolTable[K:Ordering, V] extends SymbolTable[K, V] {
  def min(): K
  def max(): K
  def floor(key: K): Option[K]
  def ceiling(key: K): Option[K]
  def rank(key: K): Int
  def select(key: Int): Option[K]
  def deleteMin(): Unit
  def deleteMax(): Unit
  def keys(lo: K, hi: K): Iterable[K]
}
