/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.geometry

import java.util.Comparator
import scala.collection.JavaConversions._
import ayla.util.UnionFind

import shapeless._
import ayla.pickling2.Pickling._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2

@SerialVersionUID(1L)
case class ScalarFunction(override val vertices: Array[Array[Float]], override val faces: Array[Array[Int]], val getFuncVal: Array[Float]) extends SimplicialComplex(vertices, faces)  {
  val minFuncVal = vertices.indices.map(getFuncVal(_)).min
  val maxFuncVal = vertices.indices.map(getFuncVal(_)).max
  val rangeFuncVal = if (maxFuncVal - minFuncVal != 0) maxFuncVal - minFuncVal else 1

  @SerialVersionUID(1L)
  class VertComparator extends java.util.Comparator[Int] with Serializable {
    @inline
    override def compare(v1: Int, v2: Int): Int = {
      val f1 = getFuncVal(v1)
      val f2 = getFuncVal(v2)

      if (f1 < f2) {
        return -1
      } else if (f1 > f2) {
        return 1
      } else if (v1 < v2) {
        return -1
      } else if (v1 > v2) {
        return 1
      } else {
        return 0
      }
    }
  }

  val vc = new VertComparator

  def this(domain: SimplicialComplex, getFuncVal: Array[Float]) = this(domain.vertices, domain.faces, getFuncVal)

  def getGradientMag(v1: Int, v2: Int): Double = {
    val d = dist(v1, v2)
    if (d == 0) {
      return 0
    }
    return math.abs(getFuncVal(v1) - getFuncVal(v2)) / d
  }

  def dist(v1: Int, v2: Int): Double = {
    // This should be faster than the cleaner version below
    var t = 0d
    vertices(v1).indices.foreach(i => {
      val diff = vertices(v1)(i) - vertices(v2)(i)
      t += (diff * diff)
    })
    //val t = (vertices(v1), vertices(v2)).zipped.map{case (a, b) => (a-b)*(a-b)}.reduceLeft(_+_)
    //		val t = vertices(v1).indices.map{i =>
    //			val diff = vertices(v1)(i) - vertices(v2)(i)
    //			diff*diff
    //		}.sum
    return math.sqrt(t)
  }

  lazy val getSteepestUpNeighborOf: Array[Int] = Array.range(0, vertices.size).map { v =>
    getLargerNeighborsOf(v).foldLeft(v)((v1, v2) => if (getGradientMag(v, v1) > getGradientMag(v, v2)) v1 else v2)
  }

  lazy val getSteepestDownNeighborOf: Array[Int] = Array.range(0, vertices.size).map { v =>
    getSmallerNeighborsOf(v).foldLeft(v)((v1, v2) => if (getGradientMag(v, v1) > getGradientMag(v, v2)) v1 else v2)
  }

  def getLargerNeighborsOf(v: Int): Iterator[Int] = getNeighbors(v).iterator.filter(n => vc.compare(v, n) < 0)
  def getSmallerNeighborsOf(v: Int): Iterator[Int] = getNeighbors(v).iterator.filter(n => vc.compare(v, n) > 0)

  //	def getLargerNeighborsOf(v: Int): Iterable[Int] = {
  //		getNeighbors(v).view.filter(n => vc.compare(v, n) < 0)
  //	}
  //	
  //	def getSmallerNeighborsOf(v: Int): Iterable[Int] = {
  //		getNeighbors(v).view.filter(n => vc.compare(v, n) > 0)
  //	}

  class StandardPersistencePair(extremum: Int, saddle: Int, killedBy: Int) extends StandardPersistencePairLike(extremum, saddle, killedBy) {
    override val persistence = math.abs(getFuncVal(extremum) - getFuncVal(saddle))
  }

  def getStandardPersistencePairs(): Array[this.StandardPersistencePair] = {
    val n = vertices.size
    val filtration = (0 until n).toArray.sortWith(vc.compare(_, _) < 0)
    val pairsSweepUp = sweep(filtration, getSmallerNeighborsOf, vc.compare(_, _))
    val pairsSweepDown = sweep(filtration.reverse, getLargerNeighborsOf, -vc.compare(_, _))

    return (pairsSweepUp ::: pairsSweepDown).toArray
  }

  private def sweep(vertFiltration: Array[Int], link: Int => Iterator[Int], comp: (Int, Int) => Int): List[StandardPersistencePair] = {
    val n = vertices.size
    val uf = new UnionFind(n) //new HashUnionFind((0 until n).toSet)
    val componentRep = new scala.collection.mutable.HashMap[Int, Int]

    val mostExtreme = vertFiltration.reduceLeft((x, y) => if (comp(x, y) < 0) x else y)
    println("Global:  " + mostExtreme)

    val extrema = new scala.collection.mutable.HashSet[Int]
    val pairs = new scala.collection.mutable.ListBuffer[StandardPersistencePair]
    vertFiltration.foreach { n1 =>
      val neighbors = link(n1).toList
      neighbors.size match {
        case 0 => {
          componentRep(n1) = n1
          extrema += n1
        }
        case 1 => {
          val n2 = neighbors.head
          val rep = componentRep(uf.find(n2))
          uf.union(n1, n2)
          componentRep(uf.find(n1)) = rep
        }
        case _ => {
          for ((n2, n3) <- neighbors.zip(neighbors.drop(1))) {
            val p2 = uf.find(n2)
            val p3 = uf.find(n3)
            if (p2 != p3) {
              // Two components merge.  Output a persistence pair
              val c = comp(componentRep(p2), componentRep(p3))
              if (c < 0) {
                pairs += new StandardPersistencePair(componentRep(p3), n1, componentRep(p2))
                uf.union(n1, n2)
                uf.union(n1, n3)
                componentRep(uf.find(n1)) = componentRep(p2)
              } else if (c > 0) {
                pairs += new StandardPersistencePair(componentRep(p2), n1, componentRep(p3))
                uf.union(n1, n2)
                uf.union(n1, n3)
                componentRep(uf.find(n1)) = componentRep(p3)
              } else {
                throw new RuntimeException
              }
            } else {
              val rep = componentRep(uf.find(n2))
              uf.union(n1, n2)
              uf.union(n1, n3)
              componentRep(uf.find(n1)) = rep
            }
          }
        }
        //case n => {throw new RuntimeException("Too many lower neighbors during merge: " + n)}
      }
    }

    println("numExtrema:" + extrema.size)

    pairs.toList
  }
}

object ScalarFunction {
  implicit def iso = Iso.hlist(apply _, unapply _)
  PicklerRegistry2.register(picklerUnpickler[ScalarFunction].create())
}

//protected[this] object ScalarFunctionHelper {
//  val vertParser = (s: String) => tokenize(s).map(t => tokenize(t).map(_.toFloat).toArray).toArray
//  val faceParser = (s: String) => tokenize(s).map(t => tokenize(t).map(_.toInt).toArray).toArray
//  val funcValParser = (s: String) => tokenize(s).map(_.toFloat).toArray
//}
//
//object ScalarFunction extends CanUnpickle(
//  ScalarFunctionHelper.vertParser :: ScalarFunctionHelper.faceParser :: ScalarFunctionHelper.funcValParser :: HNil) {
//  type CaseClass = ScalarFunction
//  implicit def iso = Iso.hlist(ScalarFunction.apply _, ScalarFunction.unapply _)
//  PickleRegistry.register(this.unpickle(_))
//}
