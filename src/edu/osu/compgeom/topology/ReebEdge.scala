package edu.osu.compgeom.topology

import org.jgrapht.graph.DefaultEdge

class ReebEdge(val internalNodes: List[Int] = List.empty[Int]) extends DefaultEdge {
	def v1 = getSource.asInstanceOf[Int]
	def v2 = getTarget.asInstanceOf[Int]
	
	def getNodeOpposite(v: Int) = {
		if (v == v1)
			v2
		else if (v == v2)
			v1
		else
			throw new IllegalArgumentException("Error:  Node " + v + " not found in edge " + (v1, v2))
	}
}