package edu.osu.compgeom.topology

import org.jgrapht.graph.SimpleGraph
import java.io.File
import java.util.Comparator
import scala.collection.JavaConversions._

class AugmentedReebGraph(val vertComparator: Comparator[Int]) extends SimpleGraph[Int, ReebEdge](Predef.classOf[ReebEdge]) {
	
	def neighborsOf(v: Int) = edgesOf(v).map(_.getNodeOpposite(v))
	def largerNeighborsOf(v: Int) = neighborsOf(v).filter(vertComparator.compare(_, v) > 0)
	def smallerNeighborsOf(v: Int) = neighborsOf(v).filter(vertComparator.compare(_, v) < 0)
}

//object AugmentedReebGraph {
//	def apply(sc: SimplicialComplex, vertComparator: Comparator[Int]): (AugmentedReebGraph, SimplicialComplex) = {
//		
//		ReebGraphExecutable.run(sc, "y", "a")
//		
//		// The output of the reeb graph executable is in the file called "out_aug.off" in the working directory
//		val augRG = new AugmentedReebGraph(vertComparator)
//		val reebSC = SimplicialComplex.fromOffFile(new File("out_aug.off"))
//		
//		(0 until reebSC.vertices.size).foreach(augRG.addVertex(_))
//		reebSC.faces.foreach(f => {
//			val i = f(0)
//			val j = f(1)
//			if (vertComparator.compare(i, j) < 0) {
//				augRG.addEdge(i, j, new ReebEdge)
//			} else {
//				augRG.addEdge(j, i, new ReebEdge)
//			}
//		})
//		
//		// Now we need the sliced simplicial complex
//		ReebGraphExecutable.run(sc, "y", "c")
//		val scSliced = SimplicialComplex.fromOffFile(new File("out_slice.off"))
//		
//		return (augRG, new SimplicialComplex(sc.vertices, scSliced.faces))
//	}
//}