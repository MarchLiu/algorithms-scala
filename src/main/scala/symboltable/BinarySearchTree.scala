package symboltable

import scala.collection.mutable;
import Ordering.Implicits._

/**
 * TODO
 *
 * @author mars
 * @version 1.0.0
 * @since 2021/05/20 10:58
 */
class BinarySearchTree[K: Ordering, V] extends SortedSymbolTable[K, V] {
  var root: Node[K, V] = null

  override def size(): Int = {
    return root.size
  }

  override def min(): K = {
    return root.min.key
  }

  override def max(): K = {
    return root.max.key
  }

  override def floor(key: K): Option[K] = {
    val node = floor(root, key)
    if (node == null) {
      return Option.empty
    }
    return Option(node.key)
  }

  def floor(node: Node[K, V], key: K): Node[K, V] = {
    if (node == null) {
      return null
    }
    if (node.key == key) {
      return node
    }

    if (key < node.key) {
      return floor(node.left, key)
    }

    val re = floor(node.right, key)
    if (re == null) {
      return node
    }
    return re
  }

  override def ceiling(key: K): Option[K] = {
    val node = ceiling(root, key)
    if (node == null) {
      return Option.empty
    }
    return Option(node.key)
  }

  def ceiling(node: Node[K, V], key: K): Node[K, V] = {
    if (node == null) {
      return null
    }
    if (node.key == key) {
      return node
    }

    if (key > node.key) {
      return floor(node.right, key)
    }

    val re = floor(node.left, key)
    if (re == null) {
      return node
    }
    return re
  }

  override def rank(key: K): Int = ???

  def rank(node: Node[K, V], key: K): Int = {
    if (node == null) {
      return 0
    }
    if (key < node.key) {
      return rank(node.left, key)
    } else if (key > node.key) {
      return 1 + node.left.size + rank(node.right, key)
    } else {
      return node.left.size
    }
  }

  override def select(i: Int): Option[K] = {
    val re = select(root, i)
    if (re == null) {
      return None
    }
    return Some(re.key)
  }

  def select(node: Node[K, V], i: Int): Node[K, V] = {
    if (node == null) {
      return null
    }
    val t = node.left.size
    if (t > i) {
      return select(node.left, i)
    } else if (t < i) {
      return select(node.right, i - t - 1)
    } else {
      return node
    }
  }

  override def deleteMin(): Unit = {
    root = root.deleteMin()
  }

  override def deleteMax(): Unit = {
    root = root.deleteMax()
  }

  override def keys(lo: K, hi: K): Iterable[K] = {
    val re: mutable.Seq[K] = mutable.Seq()
    keys(root, re, lo, hi)
    return re
  }

  def keys(node: Node[K, V], seq: mutable.Seq[K], lo: K, hi: K): Unit = {
    if (node == null) {
      return
    }
    if (lo < node.key) {
      keys(node.left, seq, lo, hi)
    }
    if (lo <= node.key && hi >= node.key) {
      seq.appended(node.key)
    }
    if (hi >= node.key) {
      keys(node, seq, lo, hi)
    }
  }

  override def put(key: K, value: V): Unit = {
    put(root, key, value)
  }

  def put(node: Node[K, V], key: K, value: V): Node[K, V] = {
    if (node == null) {
      return new Node(key, value)
    }
    if (key < node.key) {
      node.left = put(node.left, key, value)
    } else if (key > node.key) {
      node.right = put(node.right, key, value)
    } else {
      node.value = value
    }
    return node
  }

  override def get(key: K): Option[V] = {
    return get(root, key)
  }

  def get(node: Node[K, V], key: K): Option[V] = {
    if (node == null) {
      return Option.empty;
    }
    if (key == node.key) {
      return Option(node.value)
    }
    if (key < node.key) {
      return get(node.left, key)
    }
    return get(node.right, key)
  }

  override def delete(key: K): Unit = {
    root = delete(root, key)
  }

  def delete(node: Node[K, V], key: K): Node[K, V] = {
    var x = node
    if (x == null) {
      return node
    }
    if (key < x.key) {
      x.left = delete(x.left, key)
    } else if (key > node.key) {
      x.right = delete(x.right, key)
    } else {
      if (x.right == null) {
        return x.left
      }
      if (x.left == null) {
        return x.right
      }
      val t = x
      x = t.right.min
      x.right = t.right.deleteMin()
      x.left = t.left
    }
    return x

  }

  override def contains(key: K): Boolean = contains(root, key)

  def contains(node: Node[K, V], key: K): Boolean = {
    if (node == null) {
      return false
    }
    if (key < node.key) {
      return contains(node.left, key)
    }
    if (key > node.key) {
      return contains(node.right, key)
    }
    return true
  }

  override def isEmpty(): Boolean = return root.isEmpty

  override def keys(): Iterable[K] = {
    return keys(root.min.key, root.max.key)
  }
}

class Node[K: Ordering, V](val key: K, var value: V) {
  var left: Node[K, V] = null
  var right: Node[K, V] = null

  def isEmpty: Boolean = return value == null && left == null && right == null

  def size: Int = {
    var count = 1
    if (left != null) {
      count += left.size
    }
    if (right != null) {
      count += right.size
    }
    return count
  }

  def min: Node[K, V] = {
    if (left == null) {
      return this
    } else {
      return left.min
    }
  }

  def max: Node[K, V] = {
    if (right == null) {
      return this
    } else {
      return right.max
    }
  }

  def deleteMin(): Node[K, V] = {
    if (this.left == null) {
      return this.right
    }
    this.left = this.left.deleteMin()
    return this
  }

  def deleteMax(): Node[K, V] = {
    if (this.right == null) {
      return this.left
    }
    this.right = this.right.deleteMax()
    return this
  }

}