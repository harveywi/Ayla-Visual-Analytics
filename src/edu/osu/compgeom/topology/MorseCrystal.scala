package edu.osu.compgeom.topology
import org.jgrapht.graph.DefaultDirectedGraph
import org.jgrapht.graph.DefaultWeightedEdge
import org.jgrapht.alg.DijkstraShortestPath
import scala.collection.mutable.ListBuffer
import org.jgrapht.graph.SimpleDirectedGraph
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.SimpleDirectedWeightedGraph
import scala.collection.JavaConversions._

class MorseCrystal(val min: Int, val max: Int, val pts: Array[Int], val edges: Array[(Int, Int)],
		parentScalarFunction: ScalarFunction) {
	
	override def toString = "[" + (min, max).toString/* + " " + pts.mkString(",") */+"]"
	
	def toScalarFunction(parentSF: ScalarFunction): ScalarFunction = {
		val triEdges = new scala.collection.mutable.HashSet[(Int, Int)]
		for (t <- triangles) {
			for (i <- 0 to 2; j <- i + 1 to 2) {
				triEdges += new Tuple2[Int, Int](math.min(t(i), t(j)), math.max(t(i), t(j)))
			}
		}
		
		val edgesToWrite = for ((x, y) <- edges if (!triEdges.contains((math.min(x, y), math.max(x, y))))) yield (x, y)
		val edgeArray = edgesToWrite.map(t => Array[Int](t._1, t._2)).toArray
		val allFaces = Array.concat(triangles.toArray, edgeArray)
		
		val verts = (0 until parentSF.vertices.size).map(i => {
			val v = parentSF.vertices(i)
			Array(v(0), parentSF.getFuncVal(i), v(1))
		}).toArray//parentSF.vertices.map(v => Array(v(0), parentSF.getFuncVal(v)))
		
		return new ScalarFunction(new SimplicialComplex(verts, allFaces), parentSF.getFuncVal)
	}
	
	lazy val triangles: Seq[Array[Int]] = {
//		val neighborMap = HashMultimap.create[Int, Int]
		val dag = new SimpleDirectedGraph[Int, DefaultEdge](classOf[DefaultEdge])
		pts.foreach(dag.addVertex)
		edges.foreach {e =>
			val c = parentScalarFunction.vc.compare(e._1, e._2)
			if (c < 0) {
				dag.addEdge(e._1, e._2)
			} else {
				dag.addEdge(e._2, e._1)
			}
		}
		
		val triList = new ListBuffer[Array[Int]]
		dag.vertexSet.foreach {v =>
			dag.incomingEdgesOf(v).map(dag.getEdgeSource(_)).foreach {lower =>
				dag.outgoingEdgesOf(v).map(dag.getEdgeTarget(_)).foreach {upper =>
					if (dag.getEdge(lower, upper) != null) {
						triList += Array(lower, v, upper)
					}
				}
			}
		}
		
		triList.toSeq
	}
	

//	lazy val triangles: Seq[Array[Int]] = {
//		class Node(val idx: Int) extends Comparable[Node] {
//			var marked = false;
//			var deleted = false;
//			var neighbors = new scala.collection.mutable.ListBuffer[Node]
//			
//			override def compareTo(o: Node): Int = {
//				return neighbors.size.compare(o.neighbors.size)
//			}
//		}
//		val idxToNode = new java.util.HashMap[Int, Node]();
//		for (idx <- pts) {
//			idxToNode.put(idx, new Node(idx));
//		}
//		for ((x, y) <- edges) {
//			val n1 = idxToNode.get(x) 
//			val n2 = idxToNode.get(y) 
//			n1.neighbors += n2;
//			n2.neighbors += n1;
//		}
//		
//		val nodeList = new java.util.ArrayList[Node](idxToNode.values)
//		java.util.Collections.sort(nodeList, java.util.Collections.reverseOrder[Node]());
//		val ret = new scala.collection.mutable.ListBuffer[Array[Int]]
//		for (i <- 0 until nodeList.size) {
//			val vi = nodeList.get(i)
//			
//			for (neighbor <- vi.neighbors) {
//				if (!neighbor.deleted)
//					neighbor.marked = true
//			}
//			for (u <- vi.neighbors) {
//				if (u.deleted || !u.marked) {
//					// Do nothing
//				} else {
//					for (w <- u.neighbors) {
//						if (w.marked) {
//							ret += Array[Int](vi.idx, u.idx, w.idx)
//						}
//					}
//					u.marked = false
//				}
//			}
//			vi.deleted = true
//		}
//		ret;
//	}
	
	def shatter(tau: Float, crystalID: String = ""): Seq[MorseCrystal] = {
		
		val scalarFunction = toScalarFunction(parentScalarFunction)
		
		//val (rgAug, scSliced) = AugmentedReebGraph(scalarFunction, scalarFunction.vc)
		
		val rgAug = (new ReebAlgorithm(scalarFunction)).run
		//val sfSliced = new ScalarFunction(scSliced.vertices, scSliced.faces, scalarFunction.getFuncVal)
		

		import java.awt.Color
		
	  val rgSimp = SimplifiedReebGraph(rgAug, tau, scalarFunction.getFuncVal)
	  /*
	  import java.awt.Color
	  DebugUtils.getSingleton.saveDebugImage("augReeb" + crystalID + ".png",
      (g2d: java.awt.Graphics2D) => {
        g2d.setColor(new Color(Color.yellow.getRed, Color.yellow.getGreen, Color.yellow.getBlue, 80))
        for (e <- rgAug.edgeSet) {
          val p1 = DebugUtils.getSingleton.getImgCoords(scalarFunction.vertices(e.v1), 0, 2)
          val p2 = DebugUtils.getSingleton.getImgCoords(scalarFunction.vertices(e.v2), 0, 2)
          g2d.drawLine(p1.x, p1.y, p2.x, p2.y)
        }
      }
    )
    DebugUtils.getSingleton.saveDebugImage("simpReeb" + crystalID + ".png",
      (g2d: java.awt.Graphics2D) => {
        g2d.setColor(new Color(Color.yellow.getRed, Color.yellow.getGreen, Color.yellow.getBlue, 80))
        for (e <- rgSimp.edgeSet) {
          val p1 = DebugUtils.getSingleton.getImgCoords(scalarFunction.vertices(e.v1), 0, 2)
          val p2 = DebugUtils.getSingleton.getImgCoords(scalarFunction.vertices(e.v2), 0, 2)
          g2d.drawLine(p1.x, p1.y, p2.x, p2.y)
        }
      }
    )*/
	  
    println("Number contracted:  " + rgSimp.vertexSet.count(rgSimp.contract))
    /*
    DebugUtils.getSingleton.saveDebugImage("simpReebContracted" + crystalID + ".png",
      (g2d: java.awt.Graphics2D) => {
        g2d.setColor(new Color(Color.yellow.getRed, Color.yellow.getGreen, Color.yellow.getBlue, 80))
        for (e <- rgSimp.edgeSet) {
          val p1 = DebugUtils.getSingleton.getImgCoords(scalarFunction.vertices(e.v1), 0, 2)
          val p2 = DebugUtils.getSingleton.getImgCoords(scalarFunction.vertices(e.v2), 0, 2)
          val f1 = scalarFunction.getFuncVal(e.v1)
          val f2 = scalarFunction.getFuncVal(e.v2)
          println("Edge " + e + ":" + math.abs(f1 - f2))
          g2d.drawLine(p1.x, p1.y, p2.x, p2.y)
//          g2d.setColor(Color.white)
//          g2d.drawString(e.v1.toString, p1.x, p1.y)
//          g2d.drawString(e.v2.toString, p2.x, p2.y)
        }
      }
    )
    */
	  
	  val nodeToSimplifiedEdge = Array.fill[Option[ReebEdge]](rgSimp.vertexSet.size)(None)
  	for (e <- rgSimp.edgeSet;
  			 v <- e.internalNodes) {
  		nodeToSimplifiedEdge(v) = Some(e)
  	}
		
		val nodePairToBoundedReebEdge = (for (e <- rgSimp.edgeSet) yield {
			val (v1, v2) = if (scalarFunction.vc.compare(e.v1, e.v2) < 0) (e.v1, e.v2) else (e.v2, e.v1)
			((v1, v2) -> e)
		}).toMap
		
		def highestNeighborUsingTree(v: Int): Int = {
    	val next = scalarFunction.getSteepestUpNeighborOf(v)
    	if (next == v) {
    		// Local max - make sure it is not an internal node in the Reeb graph.
    		val eOpt = nodeToSimplifiedEdge(next)
	    	eOpt match {
	    		case Some(e) => {
	    			val internalNodesSorted = e.internalNodes.sortWith(scalarFunction.vc.compare(_, _) < 0)
	    			val vIdx = internalNodesSorted.indexOf(v)
	    			if (vIdx == internalNodesSorted.size - 1) {
	    				val t = internalNodesSorted(vIdx)
	    				if (rgSimp.compareVertices(t, e.v1) < 0) {
	    					return e.v1
	    				} else {
	    					assert(rgSimp.compareVertices(t, e.v2) < 0)
	    					return e.v2
	    				}
	    			} else
	    				return internalNodesSorted(vIdx+1)
	    		}
	    		case None => {
	    			return next
	    		}
	    	}    		
    	} else {
    		return next
    	}
    }
    
    def lowestNeighborUsingTree(v: Int): Int = {
    	val next = scalarFunction.getSteepestDownNeighborOf(v)
    	if (next == v) {
    		val eOpt = nodeToSimplifiedEdge(next)
	    	eOpt match {
	    		case Some(e) => {
	    			val internalNodesSorted = e.internalNodes.sortWith(scalarFunction.vc.compare(_, _) > 0)
	    			val vIdx = internalNodesSorted.indexOf(v)
	    			if (vIdx == internalNodesSorted.size - 1) {
	    				val t = internalNodesSorted(vIdx)
	    				if (rgSimp.compareVertices(t, e.v1) > 0) {
	    					return e.v1
	    				} else {
	    					assert(rgSimp.compareVertices(t, e.v2) > 0)
	    					return e.v2
	    				}
	    			}else
	    				return internalNodesSorted(vIdx+1)
	    		}
	    		case None => {
	    			return next
	    		}
	    	}    		
    	} else {
    		return next
    	}    	
    }
    
    val edgeSequence = getReebEdgeSequence(scalarFunction, lowestNeighborUsingTree(_), highestNeighborUsingTree(_), 
    		nodeToSimplifiedEdge, scalarFunction.getGradientMag(_, _))
    
    val crystalGroups = (pts).groupBy(edgeSequence)
    val debugSubcrystalToTraversedReebEdges = new scala.collection.mutable.HashMap[MorseCrystal, Set[ReebEdge]]
    val subCrystals = for ((set, groupedVertices) <- crystalGroups) yield {
    	//println("Group found: " + set)
    	val min = groupedVertices.reduceLeft((x, y) => if (scalarFunction.vc.compare(x, y) < 0) x else y)
    	val max = groupedVertices.reduceLeft((x, y) => if (scalarFunction.vc.compare(x, y) < 0) y else x)
    	
    	val vertSet = groupedVertices.toSet
    	val constrainedEdges = for (f <- scalarFunction.faces if (vertSet.contains(f(0)) && vertSet.contains(f(1)))) yield (f(0), f(1))
    	val subCrystal = new MorseCrystal(min, max, groupedVertices.toArray, constrainedEdges, scalarFunction)
    	debugSubcrystalToTraversedReebEdges.put(subCrystal, set)
    	subCrystal
    }
    println("Crystal shattered into " + subCrystals.size + " pieces.")
		
		subCrystals.toSeq
	}
	
	def getReebEdgeSequence(sc: SimplicialComplex, getSteepestNeighborUp: (Int => Int), getSteepestNeighborDown: (Int => Int), 
  		sfSliced: ScalarFunction, nodeToSimplifiedEdge: Array[Option[ReebEdge]], getGradientMag: (Int, Int) => Double,
  		nodePairToBoundedReebEdge: Map[(Int, Int), ReebEdge]): Array[Set[ReebEdge]] = {
  	
  	// Construct a steepest-ascent forest that will indicate the unique steepest path from each node to a maximum.
  	val forestUp = new DefaultDirectedGraph[Int, ReebEdge](classOf[ReebEdge])
  	(0 until sc.vertices.size).foreach(forestUp.addVertex)
  	for (v <- 0 until sc.vertices.size) {
  		val nUp = getSteepestNeighborUp(v)
  		if (v != nUp) {
  			forestUp.addEdge(v, nUp, new ReebEdge)
  		}
  	}
  	
  	val forestDown = new DefaultDirectedGraph[Int, ReebEdge](classOf[ReebEdge])
  	(0 until sc.vertices.size).foreach(forestDown.addVertex)
  	for (v <- 0 until sc.vertices.size) {
  		val nDown = getSteepestNeighborDown(v)
  		if (v != nDown) {
  			forestDown.addEdge(v, nDown, new ReebEdge)
  		}
  	}
  	
  	// Create a (directed) graph that reflects the structure of the domain, but sliced
  	// according to the structure of the Reeb graph.  Then, for each edge e = (v1, v2) in the
  	// original domain, we can find a path connecting them in the sliced domain.  e.g.,
  	// if we have a really long edge in the original domain, it might correspond to multiple
  	// edges in the Reeb graph.  This slicing trick will take care of that.
  	val sliceGraph = new SimpleDirectedWeightedGraph[Int, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
  	(0 until sfSliced.vertices.size).foreach(sliceGraph.addVertex(_))
  	(0 until sfSliced.vertices.size).foreach{v =>
  		sfSliced.getLargerNeighborsOf(v).foreach{n =>
  			val edge = sliceGraph.addEdge(v, n)
  			sliceGraph.setEdgeWeight(edge, parentScalarFunction.getFuncVal(n) - parentScalarFunction.getFuncVal(v))
  		}
  	}
  	
  	// Now we can map edge edge of the original domain to the set of Reeb graph edges that it visits
  	val domainEdgesToReebEdges = new scala.collection.mutable.HashMap[(Int, Int), Set[ReebEdge]]
  	edges.foreach{e =>
  		val (v1, v2) = if (parentScalarFunction.vc.compare(e._1, e._2) < 0) (e._1, e._2) else (e._2, e._1)
  		//val path = DijkstraShortestPath.findPathBetween(sliceGraph, v1, v2)
  		val dijkstra = new DijkstraShortestPath(sliceGraph, v1, v2/*, (scalarFunction.getFuncVal(v2) - scalarFunction.getFuncVal(v1))* 1.05*/)
  		val pathEdges = dijkstra.getPathEdgeList()
  		if (pathEdges == null) {
				import java.io._
			  val vertexIdProvider = new org.jgrapht.ext.IntegerNameProvider[Int]
  	    val vertexNameProvider = new org.jgrapht.ext.VertexNameProvider[Int] {
  				override def getVertexName(v: Int): String = {
  					if (v == v1) {
  						return v.toString + "\" style=filled color=\"#ff0000"
  					} else if (v == v2) {
  						return v.toString + "\" style=filled color=\"#00ff00"
  					} else {
  						return v.toString
  					}
  				}
  	  	}
  			val exporter = new org.jgrapht.ext.DOTExporter[Int, DefaultWeightedEdge](vertexIdProvider, vertexNameProvider, null)
  	  	val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/error.dot")))
  	  	exporter.export(bw, sliceGraph)
  	  	bw.flush
  	  	bw.close
  			
  			println("error...")
  		
  			System.exit(0)
  		}
  		val reebEdges = pathEdges.flatMap(e => {
  			val edgeSource = sliceGraph.getEdgeSource(e)
  			val edgeTarget = sliceGraph.getEdgeTarget(e)
  			
  			nodePairToBoundedReebEdge.get((edgeSource, edgeTarget)) match {
  				case Some(reebEdge) => List(Some(reebEdge))
  				case None => {
  					List(edgeSource, edgeTarget).map(nodeToSimplifiedEdge(_))
  				}
  			}
  		}).collect{case Some(e) => e}.toSet
  		domainEdgesToReebEdges.put(e, reebEdges)
  	}
  	
  	val edgesUp = treePropagate(forestUp, domainEdgesToReebEdges)
  	val edgesDown = treePropagate(forestDown, domainEdgesToReebEdges)
  	
  	val ret = new Array[Set[ReebEdge]](sc.vertices.size)
  	(0 until sc.vertices.size).foreach{v =>
  		//ret(v) = edgesDown(v)
  		ret(v) = edgesUp(v).union(edgesDown(v))
  	}
  	
//  	def rr = math.random.toFloat
//  	val simplifiedEdgeToColor = new scala.collection.mutable.HashMap[ReebEdge, java.awt.Color]
//  	
//  	for (Some(e) <- nodeToSimplifiedEdge) {simplifiedEdgeToColor.getOrElseUpdate(e, new java.awt.Color(rr, rr, rr))}
//  	
//  	val vertexIdProvider = new org.jgrapht.ext.IntegerNameProvider[Int]
//  	
//  	val vertexNameProvider = new org.jgrapht.ext.VertexNameProvider[Int] {
//			override def getVertexName(v: Int): String = {
//				nodeToSimplifiedEdge(v) match {
//					case Some(e) => {
//						val color = simplifiedEdgeToColor(e)
//						val count = ret(v).size
//						v.toString + "(" + count + ")" + "\" style=filled color=\"#" + Integer.toHexString(color.getRGB & 0x00FFFFFF)
//					}
//					case None => v.toString
//				}
//			}
//  	}
//  	
//  	import java.io._
//  	for ((fileName, forest) <- List(("/home/harveywi/up.dot", forestUp), ("/home/harveywi/down.dot", forestDown))) {
//	  	val exporter = new org.jgrapht.ext.DOTExporter[Int, ReebEdge](vertexIdProvider, vertexNameProvider, null)
//	  	val bw = new BufferedWriter(new FileWriter(new File(fileName)))
//	  	exporter.export(bw, forest)
//	  	bw.flush
//	  	bw.close  		
//  	}
  	
//  	val exporter = new org.jgrapht.ext.DOTExporter[Int, ReebEdge](vertexIdProvider, vertexNameProvider, null)
//  	val bw = new BufferedWriter(new FileWriter(new File("/Users/harvey27/t.dot")))
//  	exporter.export(bw, forestDown)
//  	bw.flush
//  	bw.close
//  	
//  	
//  	
//  	System.exit(0)
  	
  	
  	// Construct a steepest-descent forest that will indicate the unique steepest path from each node to a minimum.
  	
  	ret
  }
	
	def treePropagate(forest: DefaultDirectedGraph[Int, ReebEdge], domainEdgesToReebEdges: scala.collection.mutable.HashMap[(Int, Int), Set[ReebEdge]]): Array[Set[ReebEdge]] = {
  	
  	//val reebEdgeSets = Array.fill[Set[ReebEdge]](forest.vertexSet.size)(Set.empty)
  	val reebEdgeSets = new Array[Set[ReebEdge]](forest.vertexSet.size)
  	val it = new org.jgrapht.traverse.TopologicalOrderIterator[Int, ReebEdge](new org.jgrapht.graph.EdgeReversedGraph[Int, ReebEdge](forest))
  	
  	for (v <- it) {
  		if (forest.outgoingEdgesOf(v).size > 0) {
  			assert(forest.outgoingEdgesOf(v).size == 1)
  			val parent = forest.outgoingEdgesOf(v).iterator.next.v2
  			assert(parent != v)
  			
  			val traversedDomainEdge = forest.outgoingEdgesOf(v).iterator.next
  			 val (v1, v2) = if (parentScalarFunction.vc.compare(traversedDomainEdge.v1, traversedDomainEdge.v2) < 0)
  				(traversedDomainEdge.v1, traversedDomainEdge.v2)
  				else
  				(traversedDomainEdge.v2, traversedDomainEdge.v1)
  			reebEdgeSets(v) = reebEdgeSets(parent) ++ (domainEdgesToReebEdges.get((v1, v2)) match {
  					case Some(set) => set
  					case None => Set.empty[ReebEdge]
  				})
//  			val traversedDomainEdge = forest.outgoingEdgesOf(v).iterator.next
//  			val (v1, v2) = if (scalarFunction.vc.compare(traversedDomainEdge.v1, traversedDomainEdge.v2) < 0)
//  				(traversedDomainEdge.v1, traversedDomainEdge.v2)
//  				else
//  				(traversedDomainEdge.v2, traversedDomainEdge.v1)
//  			reebEdgeSets(v) = reebEdgeSets(traversedDomainEdge.v2) ++ domainEdgesToReebEdges((v1, v2))
  		} else {
  			reebEdgeSets(v) = Set.empty
  		}
  	}
  	return reebEdgeSets
	}
//  	for (v <- it) {
//  		if (forest.outgoingEdgesOf(v).size > 0) {
//  			assert(forest.outgoingEdgesOf(v).size == 1)
//  			val parent = forest.outgoingEdgesOf(v).iterator.next.v2
//  			//println("Parent of " + v + " is " + parent)
//  			assert(parent != v)
//  			reebEdgeSets(v) = reebEdgeSets(parent)
//  		} else {
//  			reebEdgeSets(v) = Set.empty
//  		}
//  		//println(v)
//  		nodeToSimplifiedEdge(v) match {
//  			case Some(e) => {reebEdgeSets(v) = reebEdgeSets(v) + e}
//  			case None => {/*Do nothing*/}
//  		}
//  	}
//  	return reebEdgeSets
//  }
	
	
	
	
	// OLD STUFF
		def getReebEdgeSequence(sc: SimplicialComplex, getSteepestNeighborUp: (Int => Int), getSteepestNeighborDown: (Int => Int), 
  		nodeToSimplifiedEdge: Array[Option[ReebEdge]], getGradientMag: (Int, Int) => Double): Array[Set[ReebEdge]] = {
  	
  	// Construct a steepest-ascent forest that will indicate the unique steepest path from each node to a maximum.
  	val forestUp = new DefaultDirectedGraph[Int, ReebEdge](classOf[ReebEdge])
  	(0 until sc.vertices.size).foreach(forestUp.addVertex)
  	for (v <- 0 until sc.vertices.size) {
  		val nUp = getSteepestNeighborUp(v)
  		if (v != nUp) {
  			forestUp.addEdge(v, nUp, new ReebEdge)
  		}
  	}
  	
  	val forestDown = new DefaultDirectedGraph[Int, ReebEdge](classOf[ReebEdge])
  	(0 until sc.vertices.size).foreach(forestDown.addVertex)
  	for (v <- 0 until sc.vertices.size) {
  		val nDown = getSteepestNeighborDown(v)
  		if (v != nDown) {
  			forestDown.addEdge(v, nDown, new ReebEdge)
  		}
  	}
  	
  	val edgesUp = treePropagate(forestUp, nodeToSimplifiedEdge)
  	val edgesDown = treePropagate(forestDown, nodeToSimplifiedEdge)
  	
  	val ret = new Array[Set[ReebEdge]](sc.vertices.size)
  	(0 until sc.vertices.size).foreach{v =>
  		//ret(v) = edgesDown(v)
  		ret(v) = edgesUp(v).union(edgesDown(v))
  	}
  	
//  	def rr = math.random.toFloat
//  	val simplifiedEdgeToColor = new scala.collection.mutable.HashMap[ReebEdge, java.awt.Color]
//  	
//  	for (Some(e) <- nodeToSimplifiedEdge) {simplifiedEdgeToColor.getOrElseUpdate(e, new java.awt.Color(rr, rr, rr))}
//  	
//  	val vertexIdProvider = new org.jgrapht.ext.IntegerNameProvider[Int]
//  	
//  	val vertexNameProvider = new org.jgrapht.ext.VertexNameProvider[Int] {
//			override def getVertexName(v: Int): String = {
//				nodeToSimplifiedEdge(v) match {
//					case Some(e) => {
//						val color = simplifiedEdgeToColor(e)
//						val count = ret(v).size
//						v.toString + "(" + count + ")" + "\" style=filled color=\"#" + Integer.toHexString(color.getRGB & 0x00FFFFFF)
//					}
//					case None => v.toString
//				}
//			}
//  	}
//  	
//  	import java.io._
//  	for ((fileName, forest) <- List(("/home/harveywi/up.dot", forestUp), ("/home/harveywi/down.dot", forestDown))) {
//	  	val exporter = new org.jgrapht.ext.DOTExporter[Int, ReebEdge](vertexIdProvider, vertexNameProvider, null)
//	  	val bw = new BufferedWriter(new FileWriter(new File(fileName)))
//	  	exporter.export(bw, forest)
//	  	bw.flush
//	  	bw.close  		
//  	}
  	
//  	val exporter = new org.jgrapht.ext.DOTExporter[Int, ReebEdge](vertexIdProvider, vertexNameProvider, null)
//  	val bw = new BufferedWriter(new FileWriter(new File("/Users/harvey27/t.dot")))
//  	exporter.export(bw, forestDown)
//  	bw.flush
//  	bw.close
//  	
//  	
//  	
//  	System.exit(0)
  	
  	
  	// Construct a steepest-descent forest that will indicate the unique steepest path from each node to a minimum.
  	
  	ret
  }
	
	def treePropagate(forest: DefaultDirectedGraph[Int, ReebEdge], nodeToSimplifiedEdge: Array[Option[ReebEdge]]): Array[Set[ReebEdge]] = {
  	
  	//val reebEdgeSets = Array.fill[Set[ReebEdge]](forest.vertexSet.size)(Set.empty)
  	val reebEdgeSets = new Array[Set[ReebEdge]](forest.vertexSet.size)
  	val it = new org.jgrapht.traverse.TopologicalOrderIterator[Int, ReebEdge](new org.jgrapht.graph.EdgeReversedGraph[Int, ReebEdge](forest))
////  		override def encounterVertex(vertex: Int, edge: ReebEdge): Unit = {
////  			super.encounterVertex(vertex, edge)
////  			if (edge != null) {
////  				println("Edge not null!!!!")
////  				val parent = edge.v1
////  				assert(parent != vertex)
////  				reebEdgeSets(vertex) = reebEdgeSets(parent)
////  			}
////  			//reebEdgeSets(vertex) = reebEdgeSets(vertex).add()
////  			nodeToSimplifiedEdge(vertex) match {
////  				case Some(e) => {
////  					reebEdgeSets(vertex) = reebEdgeSets(vertex) + e
////  				}
////  				case None => {/*Do nothing*/}
////  			}
////  		}
//  	}
//  	
  	// Just exhaust the iterator - all the hard work is done by overriding the encounterVertex method
  	for (v <- it) {
  		if (forest.outgoingEdgesOf(v).size > 0) {
  			assert(forest.outgoingEdgesOf(v).size == 1)
  			val parent = forest.outgoingEdgesOf(v).iterator.next.v2
  			//println("Parent of " + v + " is " + parent)
  			assert(parent != v)
  			reebEdgeSets(v) = reebEdgeSets(parent)
  		} else {
  			reebEdgeSets(v) = Set.empty
  		}
  		//println(v)
  		nodeToSimplifiedEdge(v) match {
  			case Some(e) => {reebEdgeSets(v) = reebEdgeSets(v) + e}
  			case None => {/*Do nothing*/}
  		}
  	}
  	return reebEdgeSets
  }
}