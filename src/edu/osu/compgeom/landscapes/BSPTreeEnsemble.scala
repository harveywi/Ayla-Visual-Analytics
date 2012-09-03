package edu.osu.compgeom.landscapes

import scala.annotation.tailrec
import edu.osu.compgeom.ct._
import java.io.File

import edu.osu.compgeom.util.Timing
import edu.osu.compgeom.landscapes.floorplan._

import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._

import edu.osu.compgeom.landscapes.floorplan._

import org.jgrapht.graph._
import org.jgrapht.alg._
import org.jgrapht.traverse._

@SerialVersionUID(1L)
class BSPTreeEnsemble(val roots: List[BSPNode], val nodeAtInfinity: ContourTreeNode) extends Serializable

@SerialVersionUID(1L)
class BSPNode (val ctEdge: ContourTreeEdge, 
		val outerNode: ContourTreeNode, 
		val innerNode: ContourTreeNode) extends Serializable {
	val children = new scala.collection.mutable.ArrayBuffer[BSPNode]
	var sumArea: Double = 0
}

object BSPTreeEnsemble {
  
	/**
	* Note:  nodeAtInfinity should be a node in the contracted contour tree, not the augmented tree.
	*/
	def apply(ct: ContourTree, nodeAtInfinity: ContourTreeNode): BSPTreeEnsemble = {
		val bspRoots = new ListBuffer[BSPNode]
		
		val visitedEdgesDebug = new HashSet[ContourTreeEdge]
		
		for (rootEdge <- ct.criticalNodeToIncidentEdges(nodeAtInfinity)) {
			val visitedNodes = new HashSet[ContourTreeNode]
			
			val (outerNode, innerNode) = if (rootEdge.n1 == nodeAtInfinity) (rootEdge.n1, rootEdge.n2) else (rootEdge.n2, rootEdge.n1)
			visitedNodes += outerNode
			
			val bspRoot = new BSPNode(rootEdge, outerNode, innerNode)
			bspRoots += bspRoot
			val bspNodeStack = new Stack[BSPNode]
			bspNodeStack.push(bspRoot)
			while (!bspNodeStack.isEmpty) {
				val bspNode = bspNodeStack.pop
				require(visitedNodes.contains(bspNode.ctEdge.n1) ^ visitedNodes.contains(bspNode.ctEdge.n2))
				visitedEdgesDebug += bspNode.ctEdge
				
				val (outerNode, innerNode) = if (visitedNodes.contains(bspNode.ctEdge.n1)) {
						(bspNode.ctEdge.n1, bspNode.ctEdge.n2) 
				} else {
						(bspNode.ctEdge.n2, bspNode.ctEdge.n1)
				}
				
				visitedNodes += innerNode
				
				if (!(innerNode.isMax || innerNode.isMin)) {
					val childBranches = ct.criticalNodeToIncidentEdges(innerNode).filter(!_.equals(bspNode.ctEdge))
					bspNode.children ++= childBranches.map(childBranch => {
						val (childOuterNode, childInnerNode) = 
						if (innerNode.equals(childBranch.n1))
							(childBranch.n1, childBranch.n2)
						else
							(childBranch.n2, childBranch.n1)
						new BSPNode(childBranch, childOuterNode, childInnerNode)
					})
					bspNodeStack.pushAll(bspNode.children)
				}
			}
		}
		
		val visitedEdges = new scala.collection.mutable.HashSet[ContourTreeEdge]
		val stack = new scala.collection.mutable.Stack[BSPNode]
		stack.pushAll(bspRoots)
		while (!stack.isEmpty) {
			val bspNode = stack.pop
			visitedEdges += bspNode.ctEdge
			stack.pushAll(bspNode.children)
		}
		val unexpressedEdges = ct.criticalNodeToIncidentEdges.values.flatten.filter(e => !visitedEdges.contains(e))
		val unexpressedEdgesDebug = ct.criticalNodeToIncidentEdges.values.flatten.filter(e => !visitedEdgesDebug.contains(e))
//		assert(unexpressedEdges.size == 0, "Didn't visit some edges:  " + unexpressedEdges.size)
		assert(ct.criticalNodeToIncidentEdges.values.flatten.forall(visitedEdges.contains(_)))
		
		// Make sure that all contour tree edges were visited
		assert(unexpressedEdgesDebug.size == 0, "Didn't visit some debug edges:  " + unexpressedEdgesDebug.size)
		
		// Calculate all of the areas
		bspRoots.foreach(calcArea)
		
		return new BSPTreeEnsemble(bspRoots.toList, nodeAtInfinity)
	}
	
	def calcArea(root: BSPNode) = {
	  // Parent, child
	  val stack = new Stack[BSPNode]
	  val thunks = new Stack[(BSPNode, BSPNode)]
	  stack.push(root)
	  while (!stack.isEmpty) {
	    val parent = stack.pop
	    parent.sumArea = parent.ctEdge.area
	    parent.children.foreach{child =>
	      thunks.push((parent, child))
	      stack.push(child)
	    }
	  }
	  while (!thunks.isEmpty) {
	    val t = thunks.pop
	    t._1.sumArea += t._2.sumArea
	  }
	}
}