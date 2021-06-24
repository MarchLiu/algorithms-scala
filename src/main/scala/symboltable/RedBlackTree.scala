package symboltable

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2021/06/24 14:35
 */
class RedBlackTree[K: Ordering, V] {
  import Ordering.Implicits._
  var root: RedBlackNode[K, V] = null
  def isRed(n: RedBlackNode[K, V]): Boolean = n.isRed
  def rotateLeft(n: RedBlackNode[K, V]):RedBlackNode[K, V] = {
    var re= n.right
    n.right = re.left
    re.left = n
    re.isRed = true
    return re
  }
  def rotateRight(n: RedBlackNode[K, V]):RedBlackNode[K, V] = {
    var re = n.left
    n.left = re.right
    re.right = n
    re.isRed = n.isRed
    return re
  }
  def flipColors(n: RedBlackNode[K, V]):Unit = {
    n.isRed = true
    n.left.isRed = false
    n.right.isRed = false
  }

  def size:Int = root.size

  def put(key:K, value:V):Unit = {
    root = put(root, key, value)
    root.isRed = false
  }

  def put(n: RedBlackNode[K, V], key: K, value: V): RedBlackNode[K, V] = {
    if(n == null){
      return new RedBlackNode[K, V](key, value, null, null, true)
    }

    if(key < n.key){
      n.left = put(n.left, key, value)
    } else if(key > n.key) {
      n.right = put(n.right, key, value)
    } else {
      n.value = value
    }

    var re = n
    if(isRed(re.right) && !isRed(re.left)){
      re = rotateLeft(re)
    }
    if(isRed(re.left) && isRed(re.left.left)) {
      re = rotateRight(re)
    }
    if(isRed(re.left) && isRed(re.right)) {
      flipColors(re)
    }

    return re
  }
}
