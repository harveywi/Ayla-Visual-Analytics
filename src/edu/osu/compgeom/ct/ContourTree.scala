package edu.osu.compgeom.ct

import edu.osu.compgeom.util._
import scala.util.Sorting
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.collection.JavaConversions._
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File
import edu.osu.compgeom.topology.ScalarFunction
import edu.osu.compgeom.topology.StandardPersistencePairLike

@SerialVersionUID(1L)
class ContourTree(
  val nodesAugmented: Array[ContourTreeNode],
  val nodesContracted: Array[ContourTreeNode],
  val scalarFunction: ScalarFunction,
  val criticalNodeToIncidentEdges: Map[ContourTreeNode, Array[ContourTreeEdge]]) extends Serializable {

  def simplify(tau: Float): ContourTree = {

    val faces = nodesAugmented.flatMap(child => {
      child.parents.map(parent => {
        Array(child.vertex, parent.vertex)
      })
    })

    val fakeSF = new ScalarFunction(scalarFunction.vertices, faces, scalarFunction.getFuncVal)
    val persPairs = fakeSF.getStandardPersistencePairs
    return simplify(tau, fakeSF, persPairs)
  }

  def simplify(tau: Float, fakeSF: ScalarFunction, persPairs: Array[_ <: StandardPersistencePairLike]): ContourTree = {
    val facesToAdd = persPairs.filter(_.persistence < tau).map(pair => {
      if (fakeSF.vc.compare(pair.killedBy, pair.extremum) < 0)
        Array(pair.killedBy, pair.extremum)
      else
        Array(pair.extremum, pair.killedBy)
    })

    val allFaces = (fakeSF.faces ++ facesToAdd).toArray
    return ContourTree(new ScalarFunction(scalarFunction.vertices, allFaces, scalarFunction.getFuncVal))
  }
}

@SerialVersionUID(1L)
class ContourTreeEdge(
  val n1: ContourTreeNode,
  val n2: ContourTreeNode,
  val noncriticalNodes: List[ContourTreeNode]) extends Serializable {

  var area: Double = 0
  override def toString = "(" + n1.toString + ", " + n2.toString + ")"
}

@SerialVersionUID(1L)
class ContourTreeNode(val vertex: Int) extends Serializable {
  val parents = new ArrayBuffer[ContourTreeNode]
  val children = new ArrayBuffer[ContourTreeNode]
  def isMax = (parents.size == 0 && children.size == 1)
  def isMin = (children.size == 0 && parents.size == 1)
  def isBranchUp = (parents.size > 1 && children.size == 1)
  def isBranchDown = (children.size > 1 && parents.size == 1)
  def isOrdinary = (parents.size == 1 && children.size == 1)
  def isMultibranch = (parents.size > 1 && children.size > 1)
  def isSaddle = (isBranchUp || isBranchDown)
  def isCritical = !isOrdinary
  override def toString = vertex.toString
}

/**
 *
 * Join tree has its root at the top
 * Split tree has its root at the bottom
 */
class JSTreeNode(val vertex: Int) {
  var joinParent: JSTreeNode = null
  var numJoinChildren: Int = 0

  var splitParent: JSTreeNode = null
  var numSplitChildren: Int = 0
  var deleted: Boolean = false
}

object ContourTree {
  def main(args: Array[String]): Unit = {
    val (v1, v2, v3, v4, v5, v6) = (0, 1, 2, 3, 4, 5)
    val verts = Array(v1, v2, v3, v4, v5, v6).map(i => Array(i.toFloat))
    val faces = Array(
      (v1, v2), (v1, v4), (v2, v3), (v2, v5), (v3, v6), (v4, v5), (v5, v6)).map(t => Array(t._1, t._2))
    val funcVals = Array(10, 40, 70, 60, 80, 50).map(_.toFloat)
    val sf = new ScalarFunction(verts, faces, funcVals)

    val ct = ContourTree(sf)
    ct.nodesAugmented.foreach(n => {
      println("Vertex:  " + n)
      println("Upper neighbors:  " + n.parents.mkString("(", " ", ")"))
    })
  }

  def apply(scalarFunction: ScalarFunction): ContourTree = {

    def lowerNeighbors(v: Int) = scalarFunction.getSmallerNeighborsOf(v)
    def upperNeighbors(v: Int) = scalarFunction.getLargerNeighborsOf(v)

    // First, build the list of join/split tree nodes
    val nodes = scalarFunction.vertices.indices.map(new JSTreeNode(_)).toArray

    // Build the join tree
    // (process the nodes in descending order)
    Timing("Build join tree") {
      makeJSTree(nodes,
        (n1: JSTreeNode, n2: JSTreeNode) => scalarFunction.vc.compare(n1.vertex, n2.vertex) > 0,
        upperNeighbors,
        (n1: JSTreeNode, n2: JSTreeNode) => {
          n2.joinParent = n1
          n1.numJoinChildren += 1
        })
    }

    // Build the split tree
    // (process the nodes in ascending order)
    Timing("Build split tree") {
      makeJSTree(nodes,
        (n1: JSTreeNode, n2: JSTreeNode) => scalarFunction.vc.compare(n1.vertex, n2.vertex) < 0,
        lowerNeighbors,
        (n1: JSTreeNode, n2: JSTreeNode) => {
          n2.splitParent = n1
          n1.numSplitChildren += 1
        })
    }

    Debug("Saving join tree") {
      val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/join.dot")))
      bw.write("digraph G {\n")
      for (n <- nodes if n.joinParent != null) {
        bw.write(n.vertex + " -> " + n.joinParent.vertex + "\n")
      }
      bw.write("}\n")
      bw.flush
      bw.close
    }

    Debug("Saving split tree") {
      val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/split.dot")))
      bw.write("digraph G {\n")
      for (n <- nodes if n.splitParent != null) {
        bw.write(n.vertex + " -> " + n.splitParent.vertex + "\n")
      }
      bw.write("}\n")
      bw.flush
      bw.close
    }

    // Now combine the join/split trees together to yield the (augmented) contour tree
    val nodesAugmented = Timing("Combine join/split trees") {
      combineJoinSplitTrees(nodes)
    }

    // Remove perturbation from the augmented tree
    Timing("Removing perturbation") {
      removePerturbation(nodesAugmented, scalarFunction)
    }

    // Contract the tree
    val (nodesContracted, criticalNodeToIncidentEdges) = Timing("Contracting") {
      contract(nodesAugmented)
    }

    Debug("Writing debug output file") {
      val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/cont.dot")))
      bw.write("digraph G {\n")
      for (n <- nodesContracted) {
        for (p <- n.parents) {
          bw.write(n.vertex + " -> " + p.vertex + "\n")
        }
      }
      bw.write("}\n")
      bw.flush
      bw.close
    }

    Debug("Writing debug output file") {
      val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/aug.dot")))
      bw.write("digraph G {\n")
      for (n <- nodesAugmented) {
        for (p <- n.parents) {
          bw.write(n.vertex + " -> " + p.vertex + "\n")
        }
      }
      bw.write("}\n")
      bw.flush
      bw.close
    }

    // Another sanity check:  Make sure that parent/child relationships are reflexive
    nodesAugmented.foreach { n =>
      n.children.foreach(x => require(x.parents.contains(n)))
      n.parents.foreach(x => require(x.children.contains(n)))
    }
    nodesContracted.foreach { n =>
      n.children.foreach(x => require(x.parents.contains(n)))
      n.parents.foreach(x => require(x.children.contains(n)))
    }

    return new ContourTree(nodesAugmented, nodesContracted, scalarFunction, criticalNodeToIncidentEdges)
  }

  def contract(nodesAugmented: Array[ContourTreeNode]): (Array[ContourTreeNode], Map[ContourTreeNode, Array[ContourTreeEdge]]) = {
    val nodesContracted = (for (n <- nodesAugmented.filter(_.isCritical)) yield new ContourTreeNode(n.vertex)).toArray
    val ufidToCriticalNode = new HashMap[Int, ContourTreeNode]
    val edges = new ArrayBuffer[ContourTreeEdge]

    nodesContracted.foreach(n => ufidToCriticalNode(n.vertex) = n)

    nodesAugmented.filter(_.isCritical).foreach { n =>
      n.parents.foreach { p =>
        var pCrit = p
        val visitedNodes = new ArrayBuffer[ContourTreeNode]
        visitedNodes += n
        while (!pCrit.isCritical) {
          visitedNodes += pCrit
          pCrit = pCrit.parents.head
        }
        visitedNodes += pCrit
        ufidToCriticalNode(n.vertex).parents += ufidToCriticalNode(pCrit.vertex)
        edges += new ContourTreeEdge(ufidToCriticalNode(n.vertex), ufidToCriticalNode(pCrit.vertex), visitedNodes.toList)
      }

      n.children.foreach { c =>
        var cCrit = c
        while (!cCrit.isCritical)
          cCrit = cCrit.children.head
        ufidToCriticalNode(n.vertex).children += ufidToCriticalNode(cCrit.vertex)
      }
    }

    val nodeDegrees = new HashMap[ContourTreeNode, Int]
    edges.foreach(e => {
      nodeDegrees.getOrElseUpdate(e.n1, 0)
      nodeDegrees.getOrElseUpdate(e.n2, 0)
      nodeDegrees(e.n1) += 1
      nodeDegrees(e.n2) += 1
    })

    val criticalNodeToIncidentEdges = nodeDegrees.map { case (n, degree) => (n, new Array[ContourTreeEdge](degree)) }.toMap

    edges.foreach(e => {
      nodeDegrees(e.n1) -= 1
      nodeDegrees(e.n2) -= 1
      criticalNodeToIncidentEdges(e.n1)(nodeDegrees(e.n1)) = e
      criticalNodeToIncidentEdges(e.n2)(nodeDegrees(e.n2)) = e
    })

    return (nodesContracted, criticalNodeToIncidentEdges)
  }

  def removePerturbation(nodesAugmented: Array[ContourTreeNode], scalarFunction: ScalarFunction): Unit = {
    val extrema = nodesAugmented.filter(n => n.isMax || n.isMin)

    for ((startNode, i) <- nodesAugmented.zipWithIndex if (startNode.isMax || startNode.isMin)) {
      // Gather all connected nodes with equivalent function value.
      val equivalentNodes = new ArrayBuffer[ContourTreeNode]
      val visited = new HashSet[ContourTreeNode]
      val stack = new Stack[ContourTreeNode]
      val boundaryParentList = new ArrayBuffer[ContourTreeNode]
      val boundaryChildList = new ArrayBuffer[ContourTreeNode]
      stack.push(startNode)
      while (!stack.isEmpty) {
        val n = stack.pop
        visited += n
        equivalentNodes += n

        for (parent <- n.parents if !visited.contains(parent)) {
          if (scalarFunction.getFuncVal(parent.vertex) != scalarFunction.getFuncVal(startNode.vertex)) {
            boundaryParentList += parent
            parent.children -= n
          } else {
            val x = stack.push(parent)
          }
        }

        for (child <- n.children if !visited.contains(child)) {
          if (scalarFunction.getFuncVal(child.vertex) != scalarFunction.getFuncVal(startNode.vertex)) {
            boundaryChildList += child
            child.parents -= n
          } else {
            val x = stack.push(child)
          }
        }
      }

      for (n <- equivalentNodes) {
        n.parents.clear
        n.children.clear
      }

      val equvalentNodesArray = equivalentNodes.sortWith((n1, n2) => scalarFunction.vc.compare(n1.vertex, n2.vertex) < 0).toArray

      for (i <- 0 until equvalentNodesArray.size - 1) {
        val n1 = equvalentNodesArray(i)
        val n2 = equvalentNodesArray(i + 1)
        n1.parents += n2
        n2.children += n1
      }

      val lowest = equvalentNodesArray.head
      val highest = equvalentNodesArray.last
      for (parent <- boundaryParentList) {
        parent.children += highest
        highest.parents += parent
      }

      for (child <- boundaryChildList) {
        child.parents += lowest
        lowest.children += child
      }
    }
  }

  def combineJoinSplitTrees(nodes: Array[JSTreeNode]): Array[ContourTreeNode] = {
    // Create a queue and add any nodes that are a leaf in either the join or split
    // tree.
    val ctNodes = new Array[ContourTreeNode](nodes.size)
    for (n <- nodes) {
      ctNodes(n.vertex) = new ContourTreeNode(n.vertex)
    }

    def isRemovableLeaf(n: JSTreeNode) = {
      n.numJoinChildren + n.numSplitChildren == 1
    }

    val queue = new scala.collection.mutable.Queue[JSTreeNode]()
    queue ++= nodes.filter(isRemovableLeaf)

    val undeletedNodes = new scala.collection.mutable.HashSet[JSTreeNode]
    undeletedNodes ++= nodes
    var outID = 0
    while (queue.size > 1 || (queue.size == 1 && undeletedNodes.size > 1)) {
      if (queue.size == 1)
      	println("Number of non-deleted nodes:  " + undeletedNodes.size + " " + queue.size)
      Debug("Writing debug output file") {
        val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/debug%d.dot".format(outID))))
        bw.write("digraph G {\n")
        for (n <- ctNodes) {
          for (p <- n.parents) {
            bw.write(n.vertex + " -> " + p.vertex + "\n")
          }
        }
        bw.write("}\n")
        bw.flush
        bw.close
      }
      outID += 1

      val n = queue.dequeue

      if (n.numJoinChildren == 0) {
        // Leaf in the join tree
        // Get the node's parent in the join tree
        var joinParent = n.joinParent
        while (joinParent.deleted)
          joinParent = joinParent.joinParent

        // Use path compression here
        n.joinParent = joinParent

        // Make note of the new edge to add to the contour tree
        ctNodes(n.vertex).children += ctNodes(joinParent.vertex)
        ctNodes(joinParent.vertex).parents += ctNodes(n.vertex)

        // Remove the node from the join tree
        joinParent.numJoinChildren -= 1
        n.deleted = true
        undeletedNodes -= n

        // If we have created a new (removable) leaf, then enqueue it.
        if (isRemovableLeaf(joinParent))
          queue.enqueue(joinParent)
      } else {
        // Leaf in the split tree
        // Get the node's parent in the split tree
        var splitParent = n.splitParent
        while (splitParent.deleted)
          splitParent = splitParent.splitParent

        // Use path compression here
        n.splitParent = splitParent

        // Make note of the new edge to add to the contour tree
        ctNodes(n.vertex).parents += ctNodes(splitParent.vertex)
        ctNodes(splitParent.vertex).children += ctNodes(n.vertex)

        // Remove the node from the split tree
        splitParent.numSplitChildren -= 1
        n.deleted = true
        undeletedNodes -= n

        // If we have created a new (removable) leaf, then enqueue it.
        if (isRemovableLeaf(splitParent))
          queue.enqueue(splitParent)
      }
      
      if (queue.size == 0) {
        println("Exhausted the queue!")
        val undeletedNodes = nodes.filterNot(_.deleted)
        println(undeletedNodes.forall(n => n.numJoinChildren == 1 && n.numSplitChildren == 1))
        undeletedNodes.filter(n => n.numJoinChildren != 1 || n.numSplitChildren != 1).foreach{n =>
          println("Node:  %d %d".format(n.numJoinChildren, n.numSplitChildren))
        }
        
        val toEnqueue = nodes.iterator.filter(n => !n.deleted && n.numJoinChildren == 0 && n.numSplitChildren == 0) 
        if (toEnqueue.isEmpty) {
          queue.enqueue(undeletedNodes.find(n => n.numJoinChildren == 1 && n.numSplitChildren == 1).get)
        } else {
          queue ++= toEnqueue
        }
        
      }
      
//      if (queue.size == 1) {
//        val seeds = nodes.filter(n => !n.deleted && n.numJoinChildren == 0 && n.numSplitChildren == 0)
//        queue ++= seeds
//      }
    }
    
    assert(nodes.count(n => !n.deleted) == 1, "Number of undeleted nodes:  " + nodes.count(n => !n.deleted))

    return ctNodes
  }

  def makeJSTree(
    nodes: Array[JSTreeNode],
    compareNodes: (JSTreeNode, JSTreeNode) => Boolean,
    neighbors: Int => Iterator[Int],
    updateTree: (JSTreeNode, JSTreeNode) => Unit) = {

    // Sort the tree nodes
    //Sorting.stableSort(nodes, compareNodes)
    java.util.Arrays.sort(nodes, new java.util.Comparator[JSTreeNode] {
      override def compare(n1: JSTreeNode, n2: JSTreeNode) = if (compareNodes(n1, n2)) 0 else 1
    })

    val uf = new UnionFind(nodes.size)
    val lowest = new Array[JSTreeNode](nodes.size)
    nodes.foreach(n => lowest(n.vertex) = n)

    nodes.foreach(n1 => {
      val v1 = n1.vertex
      val set1 = uf.find(v1)

      neighbors(v1).foreach(v2 => {
        val set2 = uf.find(v2)
        if (set1 != set2) {
          uf.union(set1, set2)

          // Update the tree
          if (n1 != lowest(set2))
            updateTree(n1, lowest(set2))
          lowest(set2) = n1
        }
      })
    })
  }

  //  def main(args: Array[String]): Unit = {

  //    val vertices = Array(
  //      Array(0f, 0f),
  //      Array(1f, 1f),
  //      Array(2f, 2f),
  //      Array(2f, 3f),
  //      Array(1f, 4f),
  //      Array(0f, 5f))
  //
  //    val faces = Array(
  //      Array(0, 3),
  //      Array(0, 4),
  //      Array(0, 5),
  //      Array(1, 3),
  //      Array(1, 4),
  //      Array(1, 5),
  //      Array(2, 3),
  //      Array(2, 4),
  //      Array(2, 5))
  //    
  //    
  //
  //    val sf = new ScalarFunction(vertices, faces, (i: Int) => vertices(i)(1))

  //		// Create a fake mesh
  //		val ids = List ("1", "2", "2.1", "3", "4", "4.6", "4.9", "5",
  //				"6", "6.1", "6.5", "6.9", "7", "7.2", "8", "8.3", "9", "10")
  //		
  //		val (vertices, funcVals) = for ((id, i) <- ids.zipWithIndex) 
  //			yield new Int[String, Double](id, id.toDouble, i)
  //		
  //		val vertMap = new HashMap[String, Int[String, Double]]()
  //		vertices.foreach(v => vertMap += v.id -> v)
  //		
  //		val neighborMap = Map(
  //			vertMap("1")   -> List(vertMap("2.1"), vertMap("8"), vertMap("9")),
  //			vertMap("2.1") -> List(vertMap("1"), vertMap("3"), vertMap("8"), vertMap("9")),
  //			vertMap("3")   ->	List(vertMap("2"), vertMap("5"), vertMap("6.5"),
  //					vertMap("9"), vertMap("2.1"), vertMap("8"), vertMap("6.9"), vertMap("4.9")),
  //			vertMap("2")   ->	List(vertMap("6"), vertMap("4"), vertMap("4.6"), vertMap("5"),
  //					vertMap("3"), vertMap("4.9")),
  //			vertMap("8")   ->	List(vertMap("1"), vertMap("2.1"), vertMap("3"), vertMap("6.9"), vertMap("6")),
  //			vertMap("6.9") -> List(vertMap("8"), vertMap("6"), vertMap("4.9"), vertMap("3")),
  //			vertMap("6")   ->	List(vertMap("7"), vertMap("4"), vertMap("2"), vertMap("4.9"), vertMap("6.9"), vertMap("8")),
  //			vertMap("4.9") -> List(vertMap("6"), vertMap("2"), vertMap("3"), vertMap("6.9")),
  //			vertMap("7")   ->	List(vertMap("6"), vertMap("4")),
  //			vertMap("4")   ->	List(vertMap("10"), vertMap("6.1"), 
  //													vertMap("5"), vertMap("4.6"), vertMap("2"), vertMap("6"), vertMap("7")),
  //			vertMap("9")   ->	List(vertMap("1"), vertMap("2.1"), vertMap("3"), vertMap("6.5"), vertMap("5")),
  //			vertMap("6.5") -> List(vertMap("5"), vertMap("9"), vertMap("3")),
  //			vertMap("5")   ->	List(vertMap("9"), vertMap("6.5"), vertMap("3"), vertMap("2"), vertMap("4.6"), vertMap("4"), vertMap("7.2"), vertMap("10")),
  //			vertMap("4.6") -> List(vertMap("2"), vertMap("4"), vertMap("5")),
  //			vertMap("10")  ->	List(vertMap("5"), vertMap("8.3"), vertMap("4")),
  //			vertMap("8.3") -> List(vertMap("10"), vertMap("7.2"), vertMap("6.1")),
  //			vertMap("7.2") -> List(vertMap("8.3"), vertMap("5"), vertMap("6.1")),
  //			vertMap("6.1") -> List(vertMap("4"), vertMap("8.3"), vertMap("7.2"))
  //		)
  //		
  //		def neighbors(v: Int[String, Double]) = neighborMap(v).iterator
  //		
  //		val mesh = new Mesh(
  //				vertices,
  //				(id1: String, id2: String) => id1 < id2,
  //				(f1: Double, f2: Double) => f1 < f2,
  //				neighbors,
  //				(x: Double) => x)
  //		
  //		def estimateArea(vertices: Seq[String]): Double = 1
  //		
  //		val jstree = ContourTree(mesh, estimateArea)
  //  }
}