package sort

import sort.Merge.sort
import scala.util.Random

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2021/05/07 21:44
 */
object Quick extends Sort {

  import Ordering.Implicits._

  def helper[A: Ordering](seq: Seq[A]): Seq[A] = {
    if (seq.size < 2) {
      return seq
    }

    if (seq.size < 5) {
      return Insert.sort(seq)
    }

    val p = seq.head

    val left = seq.filter(item => item < p)
    if(left.size == seq.size){
      return sort(seq.tail).appended(p)
    }

    val right = seq.filter(item => item >= p)
    if(right.size == seq.size){
      return Seq(p)++sort(seq.tail)
    }

    return helper(left) ++ helper(right)
  }

  def sort[A: Ordering](seq: Seq[A]): Seq[A] = {
    val s = Random.shuffle(seq)
    return helper(s)
  }

  def main(args: Array[String]): Unit = {
    val box: Seq[Int] = Seq[Int](9, 0, 5, 22, 29, 86, 8, 42, 32, 18, 79, 7, 86, 9, 32, 3, 2, 99)
    println(box)
    println(sort(box))
  }
}
