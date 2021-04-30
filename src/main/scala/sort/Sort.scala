package sort

import scala.collection.mutable

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2021/04/30 17:04
 */
trait Sort {
  import Ordering.Implicits._

  def exchange[A](seq: Seq[A], x: Int, y: Int): Seq[A] = {
    val left = Math.min(x, y)
    val right = Math.max(x, y)
    return seq.slice(0, left).appended(seq(right))
      ++ seq.slice(left + 1, right).appended(seq(left))
      ++ seq.slice(right + 1, seq.size)
  }

  def exchange[A](seq: mutable.Seq[A], x: Int, y: Int): Unit = {
    var tmp = seq(x)
    seq.update(x, seq(y))
    seq.update(y, tmp)
  }

  def less[A: Ordering](seq: collection.Seq[A], x: Int, y:Int): Boolean = {
    return seq(x) < seq(y)
  }

  def sorted[A: Ordering](seq: collection.Seq[A]): Boolean = {
    for(index <- 1 until seq.size){
      if(less(seq, index, index-1)){
        return false
      }
    }
    return true
  }
}
