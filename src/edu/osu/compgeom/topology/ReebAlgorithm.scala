package edu.osu.compgeom.topology

import java.io._
import scala.collection.mutable
import scala.annotation.tailrec
import scala.math.Ordered
import scala.sys.process._
import java.util.TreeSet

class ReebAlgorithm(sf: ScalarFunction) {
  def compareVertices(v1: Int, v2: Int): Int = sf.vc.compare(v1, v2)
  var edgeID = 0

  val reebEdges = new mutable.ListBuffer[Array[Int]]

  class Edge(val v1: Int, val v2: Int) {
    val parentTris = new mutable.ListBuffer[Tri]
    var numChildTris = 0
    val id = edgeID
    edgeID += 1

    var ufParent = this
    var ufRank = 0

    def find: Edge = {
      if (this == ufParent) {
        return ufParent
      } else {
        ufParent = ufParent.find
        return ufParent
      }
    }

    var obsolete = false

    override def toString = List(v1, v2, "ID:" + id).mkString("(", ",", ")")
  }

  val triCounter = new mutable.HashMap[(Int, Int, Int), Int] {
    def updateHist(key: (Int, Int, Int)): Unit = {
      put(key, getOrElseUpdate(key, 0) + 1)
    }
  }

  class Tri(val e1: Edge, val e2: Edge, val e3: Edge, val id: Int) extends Ordered[Tri] {
    e2.parentTris += this
    e1.numChildTris += 1
    e3.numChildTris += 1

    val uniqueID = {
      val theVal = ReebAlgorithm.triCounter
      ReebAlgorithm.triCounter += 1
      theVal
    }

    var deleted = false

    def delete = {
      e1.find.numChildTris -= 1
      e3.find.numChildTris -= 1
      if (e1.obsolete) {
        e1.numChildTris -= 1
      }
      if (e3.obsolete) {
        e3.numChildTris -= 1
      }
      deleted = true
    }

    def v1 = e1.v1
    def v2 = e1.v2
    def v3 = e2.v2

//    triCounter.updateHist((v1, v2, v3))

    override def compare(o: Tri): Int = {
      val c = compareVertices(v2, o.v2)
      if (c != 0) {
        c
      } else {
        uniqueID.compareTo(o.uniqueID)
      }
    }

    assert(compareVertices(v1, v2) < 0)
    assert(compareVertices(v1, v3) < 0)
    assert(compareVertices(v2, v3) < 0)

    override def toString = (if (deleted) "DELETED" else "") + "tri(id:" + uniqueID + "." + id + ")" + List(e1, e2, e3).mkString("{", ",", "}")
  }

  def union(x: Edge, y: Edge): Unit = {
    val xRoot = x.find
    val yRoot = y.find
    if (xRoot.ufRank > yRoot.ufRank) {
      yRoot.ufParent = xRoot
    } else if (xRoot.ufRank < yRoot.ufRank) {
      xRoot.ufParent = yRoot
    } else if (xRoot != yRoot) {
      yRoot.ufParent = xRoot
      xRoot.ufRank += 1
    }
  }

  def unify(e1: Edge, e2: Edge): Edge = {
    assert(e1 != e2)
    union(e1, e2)
    val rep = e1.find
    val deadEdge = if (rep == e1) e2 else e1
    // Re-assign all of the dead edge's parents and children to the representative edge.
    rep.parentTris ++= deadEdge.parentTris
    deadEdge.parentTris.clear

    rep.numChildTris += deadEdge.numChildTris
    deadEdge.obsolete = true
    return rep
  }

  def reduce(t1: Tri, t2: Tri): Option[Edge] = {
    val f1 = t1.e1.find
    val f2 = t1.e3.find
    val g1 = t2.e1.find
    val g2 = t2.e3.find

    if (f1.v2 != g1.v2) {
      val alpha = new Edge(f1.v2, g1.v2)
      val tNew1 = new Tri(alpha, f2, g2, t2.id)
      val tNew2 = new Tri(f1, g1, alpha, t1.id)

      t1.delete
      if (f2.numChildTris == 0)
        return Some(f2)
      else
        return None
    } else {
      t1.delete
      if (f1 != g1) {
        // Need to unify f1 and g1.
        val rep = unify(f1, g1)
      }
      if (f2 != g2) {
        val rep = unify(f2, g2)
      }

      return None
    }
  }

  var totalCost = 0d
  var totalTime = 0

  def run(): AugmentedReebGraph = {
    ReebAlgorithm.triCounter = 0
    val edgeSet = construct()
    
    val numTrisInit = ReebAlgorithm.triCounter
    
    var totalCost = 0d

    val stack = new mutable.Stack[Edge] {
      def addAll(args: Set[Edge]) = pushAll(args)
    }
//    val stack = new mutable.Queue[Edge] {
//      def addAll(args: Set[Edge]) = this ++= args
//      def pop = dequeue
//      def push(e: Edge) = enqueue(e)
//    }
    stack.addAll(edgeSet.filter(_.numChildTris == 0))

    val t1 = System.currentTimeMillis()
    while (!stack.isEmpty) {
      val eTerm = stack.pop

      assert(!eTerm.obsolete)

      if (eTerm.parentTris.size == 0) {
        reebEdges += Array(eTerm.v1, eTerm.v2)
      } else {
        // Reduce number of parent triangles down to 1.
//        val uniques = new mutable.ListBuffer[Tri]
//        eTerm.parentTris.groupBy(_.v2).foreach { group =>
//          val t1 = group._2.head
//          group._2.iterator.drop(1).foreach { t2 =>
//            t2.delete
//            val f1 = t1.e1.find
//            val f2 = t1.e3.find
//            val g1 = t2.e1.find
//            val g2 = t2.e3.find
//            if (f1 != g1) 
//              unify(f1, g1)
//            if (f2 != g2)
//              unify(f2, g2)
//          }
//          uniques += t1
//        }
        val trisSorted = eTerm.parentTris.sorted
        
        totalCost += trisSorted.size * math.log(trisSorted.size) / math.log(2)
        
        //        edgeValenceCounter((eTerm.v1, eTerm.v2)) = trisSorted.size
        while (trisSorted.length > 1) {
          val parent1 = trisSorted.head
          val parent2 = trisSorted(1)

          reduce(parent1, parent2) match {
            case Some(edgeToPush) => stack.push(edgeToPush)
            case None => { /*Do nothing*/ }
          }
          trisSorted.trimStart(1)
          //          eTerm.parentTris.trimStart(1)
          parent1.deleted = true
        }
        // Now the edge should have exactly one parent triangle
        val parentTri = trisSorted.head
        parentTri.delete
        eTerm.parentTris.clear
        val e1 = parentTri.e1.find
        val e3 = parentTri.e3.find
        if (e1.numChildTris == 0)
          stack.push(e1)
        if (e3.numChildTris == 0)
          stack.push(e3)
      }
    }
    val t2 = System.currentTimeMillis()
    totalTime = (t2 - t1).toInt

    val faces = reebEdges.toArray

    println("Number of Reeb edges:  " + reebEdges.length)
    val trisCreated = ReebAlgorithm.triCounter - numTrisInit
    println("\tNumber of tris created:  (%d %f %f)".format(trisCreated, numTrisInit*math.log(numTrisInit)/math.log(2), trisCreated / (numTrisInit*math.log(numTrisInit)/math.log(2))))
    println("Total cost:  " + totalCost)
    println("t lg t:      " + (numTrisInit*math.log(numTrisInit)/math.log(2)))
    val ratio = totalCost / (numTrisInit*math.log(numTrisInit)/math.log(2))
    println("Ratio:       " + ratio)
    //    outSC.toOffFile(new File("/dev/shm/out.off"))

    val ret = new AugmentedReebGraph(sf.vc)
    (0 until sf.vertices.size).foreach(ret.addVertex(_))
    faces.foreach(f => {
      val i = f(0)
      val j = f(1)
      if (sf.vc.compare(i, j) < 0) {
        ret.addEdge(i, j, new ReebEdge)
      } else {
        ret.addEdge(j, i, new ReebEdge)
      }
    })
    
    return ret
  }

  def construct(): Set[Edge] = {
    // Construct the graph
    val edgeMap = new mutable.HashMap[(Int, Int), Edge]
    val triList = new mutable.ListBuffer[Tri]
    var id = 0
    sf.faces.foreach { f =>
      if (f.size == 2) {
        reebEdges += f
      } else if (f.size == 3) {
        val fs = f.sortWith(compareVertices(_, _) < 0)
        val v1 = fs(0)
        val v2 = fs(1)
        val v3 = fs(2)

        val e1 = edgeMap.getOrElseUpdate((v1, v2), new Edge(v1, v2))
        val e2 = edgeMap.getOrElseUpdate((v1, v3), new Edge(v1, v3))
        val e3 = edgeMap.getOrElseUpdate((v2, v3), new Edge(v2, v3))

        val t = new Tri(e1, e2, e3, id)
        id += 1
      } else {
        throw new RuntimeException("Unsupported face:  " + f.mkString("(", " ", ")"))
      }
    }
    assert(edgeMap.values.map(_.id).toSet.size == edgeMap.size)
    return edgeMap.values.toSet
  }

}

object ReebAlgorithm {

  var triCounter = 0
}

