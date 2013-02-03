/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.util

class UnionFind(n: Int) {
  val parents = Array.range(0, n)
  val ranks = new Array[Int](n)

  def union(x: Int, y: Int): Unit = {
    val xRoot = find(x)
    val yRoot = find(y)
    if (ranks(xRoot) > ranks(yRoot)) {
      parents(yRoot) = xRoot
    } else if (ranks(xRoot) < ranks(yRoot)) {
      parents(xRoot) = yRoot
    } else if (xRoot != yRoot) {
      parents(yRoot) = xRoot
      ranks(xRoot) += 1
    }
  }

  def find(x: Int): Int = {
    if (parents(x) == x)
      x
    else {
      parents(x) = find(parents(x))
      parents(x)
    }
  }
}

class HashUnionFind[T](initialSet: Set[T] = Set.empty[T]) {
  val parents = new scala.collection.mutable.HashMap[T, T]
  val ranks = new scala.collection.mutable.HashMap[T, Int]

  for (x <- initialSet) {
    makeSet(x)
  }

  def makeSet(x: T) = {
    parents(x) = x
    ranks(x) = 0
  }

  def this() = this(Set.empty[T])

  def union(x: T, y: T): T = {
    val xRoot = find(x)
    val yRoot = find(y)
    if (ranks(xRoot) > ranks(yRoot)) {
      parents(yRoot) = xRoot
    } else if (ranks(xRoot) < ranks(yRoot)) {
      parents(xRoot) = yRoot
    } else if (xRoot != yRoot) {
      parents(yRoot) = xRoot
      ranks(xRoot) += 1
    }
    return find(x)
  }

  def findOpt(x: T): Option[T] = {
    parents.get(x) match {
      case Some(p) => {
        if (p == x) {
          Some(x)
        } else {
          parents(x) = find(parents(x))
          Some(parents(x))
        }
      }
      case None => None
    }
  }

  /**
   * Automatically performs a makeSet operation if x is not found.
   */
  def find(x: T): T = {
    parents.get(x) match {
      case Some(p) => {
        if (p == x) {
          x
        } else {
          parents(x) = find(parents(x))
          parents(x)
        }
      }
      case None => {
        // Make set
        parents(x) = x
        ranks(x) = 0
        x
      }
    }
  }
}
