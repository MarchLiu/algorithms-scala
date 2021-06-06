package symboltable

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2021/05/12 12:06
 */
trait SymbolTable[K:Ordering, V] {
  def put(key: K, value: V): Unit;
  def get(key: K): Option[V];
  def delete(key: K): Unit;
  def contains(key: K): Boolean;
  def isEmpty(): Boolean;
  def size(): Int;
  def keys(): Iterable[K];
}
