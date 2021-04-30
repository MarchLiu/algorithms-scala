package sort

import sort.Select.sort

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2021/04/30 14:46
 */
object Insert {

  import Ordering.Implicits._

  def insert[A: Ordering](seq: Seq[A], element: A): Seq[A] = {
    if (seq.isEmpty) {
      return seq.appended(element)
    }
    if (seq.size == 1) {
      if (seq.head < element) {
        return seq.appended(element)
      } else {
        return Seq(element) ++ seq
      }
    }
    for (point <- 0 until seq.size - 1) {
      if (seq(point) < element && !(element > seq(point + 1))){
        return seq.slice(0, point).appended(element) ++ seq.slice(point, seq.size)
      }
    }
    if(element < seq.last ){
      return seq.dropRight(1).appended(element).appended(seq.last)
    }

    return seq.appended(element)
  }

  def sort[A: Ordering](seq: Seq[A]): Seq[A] = {
    if (seq.size < 2) {
      return seq
    }

    var result: Seq[A] = Seq()

    for(item <- seq){
      result = insert(result, item)
    }

    result
  }

  def main(args: Array[String]): Unit = {
    val box:Seq[Int] = Seq[Int](0, 5, 8, 7, 9, 3, 2, 99)
    println(box)
    sort(box)
    println(box)
  }
}
