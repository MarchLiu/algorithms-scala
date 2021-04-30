package sort

import scala.collection.mutable

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2021/04/29 10:17
 */
object Select {
  import Ordering.Implicits._

  def sort[A: Ordering](seq: mutable.Seq[A]): Unit = {
    if (seq.size < 2) {
      return
    }

    for (i <- 0 until seq.size) {
      var min = i
      for (j <- i + 1 until seq.size) {
        if (seq(j) < seq(min)) {
          min = j
        }
      }

      if (min != i) {
        var tmp = seq(min)
        seq.update(min, seq(i))
        seq.update(i, tmp)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val box:mutable.Seq[Int] = mutable.Seq[Int](0, 5, 7, 3, 2, 99)
    println(box)
    sort(box)
    println(box)
  }
}