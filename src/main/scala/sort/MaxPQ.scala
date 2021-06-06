package sort

import sort.Quick.sort

import scala.collection.mutable
import scala.util.Random

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
  val poolSize = 5
  def main(args: Array[String]): Unit = {
    val pq = new MaxPQ[Int]()
    val data:Seq[Int] = for(_ <- 0 to 19) yield Random.nextInt(100)
    println(data)
    for (item <- data) {
      pq.insert(item)
      if (pq.buffer.length > poolSize) {
        pq.delMax()
      }
    }
    println(pq.buffer)
    println(pq.buffer.sorted.toSeq)
    val sorted = data.sorted.take(poolSize)
    println(sorted)
  }
}