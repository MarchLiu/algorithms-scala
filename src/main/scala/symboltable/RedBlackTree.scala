package symboltable

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2021/06/24 14:35
 */
class RedBlackTree[K: Ordering, V] {
  type Node = RedBlackNode[K, V]

  import Ordering.Implicits._

  var root: Node = null

  def isRed(n: Node): Boolean = n != null && n.isRed

  def get(k: K) : V = {
    return get(root, k)
  }

  def get(node: Node, k: K): V = {
    if(k < node.key){
      return get(node.left, k)
    }
    if(k > node.key){
      return get(node.right, k)
    }

    return node.value
  }

  def rotateLeft(n: Node): Node = {
    var re = n.right
    n.right = re.left
    re.left = n
    re.isRed = true
    return re
  }

  def rotateRight(n: Node): Node = {
    var re = n.left
    n.left = re.right
    re.right = n
    re.isRed = n.isRed
    return re
  }

  def flipColors(n: Node): Unit = {
    n.isRed = true
    n.left.isRed = false
    n.right.isRed = false
  }

  def size: Int = {
    if (root == null) {
      return 0
    }
    root.size
  }

  def isEmpty: Boolean = size == 0

  def put(key: K, value: V): Unit = {
    root = put(root, key, value)
    root.isRed = false
  }

  def put(n: Node, key: K, value: V): Node = {
    if (n == null) {
      return new Node(key, value, null, null, true)
    }

    if (key < n.key) {
      n.left = put(n.left, key, value)
    } else if (key > n.key) {
      n.right = put(n.right, key, value)
    } else {
      n.value = value
    }

    var re = n
    if (isRed(re.right) && !isRed(re.left)) {
      re = rotateLeft(re)
    }

    if (isRed(re.left) && isRed(re.left.left)) {
      re = rotateRight(re)
    }
    if (isRed(re.left) && isRed(re.right)) {
      flipColors(re)
    }

    return re
  }

  def delete(key: K): Unit = {
    if (root == null) {
      return
    }

    if (!isRed(root.left) && !isRed(root.right)) {
      root.isRed = true;
    }
    root = delete(root, key)
    if (root != null && root.size > 0) {
      root.isRed = false
    }
  }

  def delete(n: Node, k: K): Node = {
    var re = n
    if (k < re.key) {
      re = rotateRight(re)
    }
    if (k == re.key && re.right == null) {
      return null;
    }
    if (k < re.key) {
      if (!isRed(re.right) && !isRed(re.right.left)) {
        re = moveRedLeft(re)
      }
      re.left = delete(re.left, k)
    } else {
      if (isRed(re.left)) {
        re = rotateRight(re)
      }
      if (k == re.key && re.right == null) {
        return null
      }
      if (!isRed(re.right) && !isRed(re.right.left)) {
        re = moveRedRight(re)
      }
      if (k == re.key) {
        var m = min(re.right)
        re.value = get(re.right, m.key)
        re.key = m.key
        re.right = deleteMin(re.right)
      } else {
        re.right = delete(re.right, k)
      }
    }
    return balance(re)
  }

  def deleteMin(): Unit = {
    if (!isRed(root.left) && !isRed(root.right)) {
      root.isRed = true
    }
    root = deleteMin(root)
    if (!isEmpty) {
      root.isRed = false
    }
  }

  def deleteMin(n: Node): Node = {
    var re = n
    if (re.left == null) {
      return null;
    }
    if (!isRed(re.left) && !isRed(re.left.left)) {
      re = moveRedLeft(re)
    }
    re.left = deleteMin(n)
    return balance(re)
  }

  def deleteMax(): Unit = {
    if (!isRed(root.left) && !isRed(root.right)) {
      root.isRed = true
    }
    root = deleteMax(root)
    if (!isEmpty) {
      root.isRed = false
    }
  }

  def deleteMax(n: Node): Node = {
    var re = n
    if (isRed(re.left)) {
      re = rotateRight(n)
    }
    if (re.right == null) {
      return null
    }
    if (!isRed(re.right) && !isRed(re.right.left)) {
      re = moveRedRight(re)
    }
    re.right = deleteMax(re.right)
    return balance(re)
  }

  def balance(node: Node): Node = {
    var re = node
    if (isRed(re.right)) {
      re = rotateLeft(re)
    }

    if (isRed(re.right) && !isRed(re.left)) {
      re = rotateLeft(re)
    }

    if (isRed(re.left) && isRed(re.left.left)) {
      re = rotateRight(re)
    }
    if (isRed(re.left) && isRed(re.right)) {
      flipColors(re)
    }

    return re
  }

  def min: Node = {
    return min(root)
  }

  def min(n: Node): Node = {
    if(n.left == null) {
      return n
    }
    return min(n)
  }

  def max: Node = {
    return max(root)
  }

  def max(n: Node): Node = {
    if(n.left == null) {
      return n
    }
    return max(n)
  }

  def moveRedLeft(n: Node): Node = {
    var re = n
    flipColors(re)
    if (isRed(re.right.left)) {
      re.right = rotateRight(re.right)
      re = rotateLeft(re)
    }
    return re
  }

  def moveRedRight(n: Node): Node = {
    var re = n
    flipColors(re)
    if (!isRed(n.left.left)) {
      re = rotateRight(re)
    }
    return re
  }
}
