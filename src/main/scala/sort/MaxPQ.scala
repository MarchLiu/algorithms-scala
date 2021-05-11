package sort

import sort.Quick.sort

import scala.collection.mutable

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2021/05/11 15:54
 */
class MaxPQ[E: Ordering] extends Stack[E] {
  def insert(item: E): Unit = {
    buffer = buffer.appended(item)
    swim(buffer.size - 1)
  }

  def delMax(): E = {
    val last = buffer.size - 1
    exchange(0, last)
    val max = buffer(0)
    buffer = buffer.dropRight(1);
    sink(0)
    max
  }
}

object MaxPQ {
  def main(args: Array[String]): Unit = {
    val pq = new MaxPQ[Int]()
    val data = Seq[Int](9, 0, 5, 22, 29, 86, 8, 42, 32, 18, 79, 7, 86, 9, 32, 3, 2, 99)
    println(data)
    for (item <- data) {
      pq.insert(item)
      if (pq.buffer.length > 7) {
        pq.delMax()
      }
    }
    println(pq.buffer)
  }
}