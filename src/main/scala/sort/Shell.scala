package sort

import scala.collection.mutable

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2021/04/30 16:52
 */
object Shell extends Sort {

  import Ordering.Implicits._

  def sort[A: Ordering](seq: mutable.Seq[A]): Unit = {
    val s = seq.size
    var h = 1;
    while (h < s / 3) {
      h = 3 * h + 1;
    }

    while (h >= 1) {
      for (i <- h until s) {
        var j = i
        while(j >= h && less(seq, j, j-h)) {
          exchange(seq, j, j-h)
          j -= h
        }
      }
      h = h/3
    }
  }

  def main(args: Array[String]): Unit = {
    val box:mutable.Seq[Int] = mutable.Seq[Int](0, 5, 8, 7, 9, 3, 2, 99)
    println(box)
    sort(box)
    println(box)
  }
}
