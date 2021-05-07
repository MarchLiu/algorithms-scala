package sort

import sort.Insert.sort

import scala.collection.mutable

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2021/05/07 20:58
 */
object Merge extends Sort {
  import Ordering.Implicits._

  def merge[A:Ordering](left: Seq[A], right: Seq[A]): Seq[A] = {
    if(left.isEmpty){
      return right
    }
    if(right.isEmpty){
      return left
    }

    var seql = sort(left)
    var seqr = sort(right)

    var hl = seql.head
    var hr = seqr.head

    var tail = merge(seql.tail, seqr.tail)
    return sort(Seq(hl, hr)) ++ tail
  }

  def sort[A:Ordering](seq:Seq[A]):Seq[A]= {
    if(seq.size < 2){
      return seq
    }

    if(seq.size == 2) {
      if(seq.head > seq.last){
        return seq.reverse
      } else {
        return seq
      }
    }

    val idx = seq.size / 2
    return merge(seq.slice(0, idx), seq.slice(idx, seq.size))
  }

  def main(args: Array[String]): Unit = {
    val box: Seq[Int] = Seq[Int](0, 5, 8, 7, 86, 9, 32, 3, 2, 99)
    println(box)
    println(sort(box))
  }
}
