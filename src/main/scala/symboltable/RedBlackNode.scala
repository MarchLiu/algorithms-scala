package symboltable

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2021/06/24 14:38
 */
class RedBlackNode[K: Ordering, V](var key: K, var value: V,
                                     var left: RedBlackNode[K, V],
                                     var right: RedBlackNode[K, V],
                                     var isRed: Boolean) {

  def size: Int = {
    var c = 0
    if(value != null){
      c += 1
    }
    if(left !=null) {
      c += left.size
    }
    if(right != null) {
      c += right.size
    }
    return c
  }
  def isBlack: Boolean = !isRed
}
