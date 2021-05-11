package sort

import scala.collection.mutable;

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2021/05/11 14:55
 */
class Stack[E: Ordering] {
  var buffer: mutable.Seq[E] = mutable.Seq[E]()

  import Ordering.Implicits._

  def less(i: Int, j: Int): Boolean = buffer(i) < buffer(j)

  def exchange(i: Int, j: Int): Unit = {
    var tmp = buffer(i)
    buffer.update(i, buffer(j))
    buffer.update(j, tmp)
  }

  def swim(k: Int): Unit = {
    var idx = k
    while (idx >= 1 && less(idx / 2, idx)) {
      exchange(idx / 2, idx)
      idx = idx / 2
    }
  }

  def sink(k: Int): Unit = {
    var idx = k
    val last = buffer.size - 1
    while (2 * idx <= last) {
      var j = 2 * idx
      if (j < last && less(j, j + 1)) {
        j += 1
      }
      if (!less(idx, j)) {
        return
      }
      exchange(idx, j)
      idx = j
    }
  }
}
