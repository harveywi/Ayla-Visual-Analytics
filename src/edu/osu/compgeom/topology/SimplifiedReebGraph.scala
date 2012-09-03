package edu.osu.compgeom.topology

import java.io._
import java.util.Comparator
import org.jgrapht.graph._
import org.jgrapht.alg.DijkstraShortestPath
import org.jgrapht.ext.GraphMLExporter
import edu.osu.compgeom.util.HashUnionFind
import scala.collection.JavaConversions._

class SimplifiedReebGraph(val vertComparator: Comparator[Int], getFuncVal: Int => Float) extends DirectedMultigraph[Int, ReebEdge](classOf[ReebEdge]) {

  def smallerNeighborsOf(v: Int) = incomingEdgesOf(v).map(getEdgeSource(_))
  def largerNeighborsOf(v: Int) = outgoingEdgesOf(v).map(getEdgeTarget(_))
  def compareVertices(v1: Int, v2: Int) = vertComparator.compare(v1, v2)

  def contract(v: Int): Boolean = {
    val lnSet = largerNeighborsOf(v)
    val snSet = smallerNeighborsOf(v)

    assert(lnSet.forall(vertComparator.compare(v, _) < 0))
    assert(snSet.forall(vertComparator.compare(_, v) < 0))

    if ((lnSet.size == 1) && (snSet.size == 1)) {
      val ln = lnSet.toList.head
      val sn = snSet.toList.head
      val lowEdge = getEdge(sn, v)
      val highEdge = getEdge(v, ln)

      val mergedInternalNodes = v :: lowEdge.internalNodes ::: highEdge.internalNodes
      val contractedEdge = new ReebEdge(mergedInternalNodes)

      assert(removeEdge(lowEdge))
      assert(removeEdge(highEdge))
      assert(addEdge(sn, ln, contractedEdge))
      return true
    }
    return false
  }
  
  def toAugmentedReebGraph: AugmentedReebGraph = {
  	val newRG = new AugmentedReebGraph(vertComparator)
		vertexSet.foreach(newRG.addVertex(_))
  	edgeSet.foreach(e => {
			val internalNodesSorted = e.internalNodes.sortWith(vertComparator.compare(_, _) < 0)
			val allNodesOnEdge = IndexedSeq(e.v1) ++ internalNodesSorted ++ IndexedSeq(e.v2)
			val n = allNodesOnEdge.size
			for (i <- 0 until n-1; j = i + 1) {
				val v1 = allNodesOnEdge(i)
				val v2 = allNodesOnEdge(j)
				newRG.addEdge(v1, v2, new ReebEdge)
			}
		})
		
		return newRG
  }

  def getStandardPersistencePairs: List[StandardPersistencePair] = {
    val n = vertexSet.size
    val filtration = vertexSet.toSeq.sortWith(compareVertices(_, _) < 0)
    println("sweeping up")
    val pairsSweepUp = sweep(filtration, smallerNeighborsOf, compareVertices)
    println("sweeping down")
    
    def reverseCompare(i: Int, j: Int) = compareVertices(j, i)
    val filtration2 = vertexSet.toSeq.sortWith(reverseCompare(_, _) < 0)
    val pairsSweepDown = sweep(filtration2, largerNeighborsOf, reverseCompare)

    return pairsSweepUp ::: pairsSweepDown
  }

  override def addEdge(v1: Int, v2: Int, e: ReebEdge): Boolean = {
    assert(compareVertices(v1, v2) < 0)
    return super.addEdge(v1, v2, e)
  }

  //	override def addEdge(v1: Int, v2: Int, e: ReebEdge): Boolean = {
  //		if (compareVertices(v1, v2) < 0)
  //			super.addEdge(v1, v2, e)
  //		else
  //			super.addEdge(v2, v1, e)
  //	}

  override def getEdge(v1: Int, v2: Int): ReebEdge = {
    if (compareVertices(v1, v2) < 0)
      super.getEdge(v1, v2)
    else
      super.getEdge(v2, v1)
  }

  private def sweep(vertFiltration: Seq[Int], link: Int => Iterable[Int], comp: (Int, Int) => Int): List[StandardPersistencePair] = {
    val n = vertexSet.size
    val uf = new HashUnionFind((0 until n).toSet)
    val componentRep = new scala.collection.mutable.HashMap[Int, Int]

    val pairs = new scala.collection.mutable.ListBuffer[StandardPersistencePair]
    for (n1 <- vertFiltration) {
      val neighbors = link(n1).toList
      neighbors.size match {
        case 0 => {
          componentRep(n1) = n1
        }
        case 1 => {
          val n2 = neighbors.head
          val rep = componentRep(uf.find(n2))
          uf.union(n1, n2)
          componentRep(uf.find(n1)) = rep
        }
        case _ => {
        	// The components should be killed in a "domino" order.
        	val neighborRepPairs = neighbors.zip(neighbors.map(n => componentRep(uf.find(n))))
        	val neighborRepPairsSorted = neighborRepPairs.sortWith((pair1, pair2) => comp(pair1._2, pair2._2) < 0)
        	val neighborsSorted = neighborRepPairsSorted.map(_._1)
        	
        	// n3 is shallower than n2
        	val newPairs = neighborsSorted.reverse.sliding(2).flatMap(pair => {
        		val n3 = pair(0)
        		val n2 = pair(1)
        		val p2 = uf.find(n2)
        		val p3 = uf.find(n3)
        		
        		if (p2 != p3) {
        			// Two components merge.  Output a persistence pair
              assert(comp(componentRep(p2), componentRep(p3)) < 0)
              val newPair = new StandardPersistencePair(componentRep(p3), n1, componentRep(p2))
              uf.union(n1, n2)
              uf.union(n1, n3)
              componentRep(uf.find(n1)) = componentRep(p2)
              Some(newPair)
        		} else {
        			 // p2 and p3 are already connected.
              val rep = componentRep(uf.find(n2))
              uf.union(n1, n2)
              uf.union(n1, n3)
              componentRep(uf.find(n1)) = rep
              None
        		}
        	})
        	
        	pairs ++= newPairs
        	
        	/*
          // What do we want to do here?  The component with the lowest representative should kill all of the
          // other components.

          val neighborRepPairs = neighbors.zip(neighbors.map(n => componentRep(uf.find(n))))
          val neighborRepPairsSorted = neighborRepPairs.sortWith((pair1, pair2) => comp(pair1._2, pair2._2) < 0)

          val neighborsSorted = neighborRepPairsSorted.map(_._1)
          val n2 = neighborsSorted.head

          for (n3 <- neighborsSorted.tail) {
            val p2 = uf.find(n2)
            val p3 = uf.find(n3)
            if (p2 != p3) {
              // Two components merge.  Output a persistence pair
              assert(comp(componentRep(p2), componentRep(p3)) < 0)
              pairs += new StandardPersistencePair(componentRep(p3), n1, componentRep(p2))
              uf.union(n1, n2)
              uf.union(n1, n3)
              componentRep(uf.find(n1)) = componentRep(p2)
            } else {
              // p2 and p3 are already connected.
              val rep = componentRep(uf.find(n2))
              uf.union(n1, n2)
              uf.union(n1, n3)
              componentRep(uf.find(n1)) = rep
            }
          }
          */

          //					val neighborRepPairs = neighbors.zip(neighbors.map(n => componentRep(uf.find(n))))
          //					val lowest = neighborRepPairs.reduceLeft((pair1, pair2) => if (comp(pair1._2, pair2._2) < 0) pair1 else pair2)

          //					val ns = neighbors.sortWith(comp(_, _) < 0)
          //					for ((n2, n3) <- ns.zip(ns.drop(1))) {
          //						val p2 = uf.find(n2)
          //						val p3 = uf.find(n3)
          //						if (p2 != p3) {
          //							// Two components merge.  Output a persistence pair
          //							val c = comp(componentRep(p2), componentRep(p3))
          //							if (c < 0) {
          //								pairs += new StandardPersistencePair(componentRep(p3), n1, componentRep(p2))
          //								uf.union(n1, n2)
          //								uf.union(n1, n3)
          //								componentRep(uf.find(n1)) = componentRep(p2)
          //							} else if (c > 0) {
          //								pairs += new StandardPersistencePair(componentRep(p2), n1, componentRep(p3))
          //								uf.union(n1, n2)
          //								uf.union(n1, n3)
          //								componentRep(uf.find(n1)) = componentRep(p3)
          //							} else {
          //								throw new RuntimeException
          //							}
          //						} else {
          //							val rep = componentRep(uf.find(n2))
          //							uf.union(n1, n2)
          //							uf.union(n1, n3)
          //							componentRep(uf.find(n1)) = rep
          //						}
          //					}
        }
      }
    }

    pairs.toList
  }

  def writeDotFile(f: File): Unit = {
    val bw = new BufferedWriter(new FileWriter(f))
    bw.write("Digraph G {\n")
    for (e <- edgeSet) {
      if (compareVertices(e.v1, e.v2) < 0) {
        bw.write(List(e.v1, e.v2).mkString("\t", " -> ", "\n"))
      } else {
        bw.write(List(e.v2, e.v1).mkString("\t", " -> ", "\n"))
      }
    }
    bw.write("}\n")
    bw.flush
    bw.close

  }

  sealed abstract case class PersistencePair(val extremum: Int, val saddle: Int) extends Ordered[PersistencePair] {
    val persistence = math.abs(getFuncVal(saddle) - getFuncVal(extremum))

    override def compare(o: PersistencePair): Int = {
      if (persistence == o.persistence) {
        val range1 = math.abs(extremum - saddle)
        val range2 = math.abs(o.extremum - o.saddle)
        printf("(%d %d) %d %d %d %d\n", range1, range2, extremum, saddle, o.extremum, o.saddle)
        return range1.compare(range2)
        //throw new RuntimeException
      } else {
        return persistence.compare(o.persistence)
      }
      //persistence.compare(o.persistence)
    }

    def cancel: Option[Set[ReebEdge]]
  }
  
  class StandardPersistencePair(extremum: Int, saddle: Int, val killedBy: Int) extends PersistencePair(extremum, saddle) {
  	assert((compareVertices(killedBy, extremum) < 0 && compareVertices(extremum, saddle) < 0 && compareVertices(killedBy, saddle) < 0) ||
  			(compareVertices(saddle, extremum) < 0 && compareVertices(extremum, killedBy) < 0 && compareVertices(saddle, killedBy) < 0))
  			
    //		val (source1, dest1) = if (compareVertices(extremum, saddle) < 0) (extremum, saddle) else (saddle, extremum)
    //		val path1_test = DijkstraShortestPath.findPathBetween(SimplifiedReebGraph.this, source1, dest1)
    //		
    //		if (path1_test == null) {
    //			println("Huge error1!")
    //			val vertexIdProvider = new org.jgrapht.ext.IntegerNameProvider[Int]
    //	    val vertexNameProvider = new org.jgrapht.ext.VertexNameProvider[Int] {
    //				override def getVertexName(v: Int): String = {
    //					if (v == saddle) {
    //						return v.toString + "\" style=filled color=\"#0000ff"
    //					} else if (v == extremum) {
    //						return v.toString + "\" style=filled color=\"#ff0000"
    //					} else if (v == killedBy) {
    //						return v.toString + "\" style=filled color=\"#00ff00"
    //					} else {
    //						return v.toString
    //					}
    //				}
    //	  	}
    //			val exporter = new org.jgrapht.ext.DOTExporter[Int, ReebEdge](vertexIdProvider, vertexNameProvider, null)
    //	  	val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/t.dot")))
    //	  	exporter.export(bw, SimplifiedReebGraph.this)
    //	  	bw.flush
    //	  	bw.close
    //			System.exit(0)
    //		}
    //		
    //		val (source2, dest2) = if (compareVertices(killedBy, saddle) < 0) (killedBy, saddle) else (saddle, killedBy)
    //		val path2_test = DijkstraShortestPath.findPathBetween(SimplifiedReebGraph.this, source2, dest2)
    //		
    //		if (path2_test == null) {
    //			val vertexIdProvider = new org.jgrapht.ext.IntegerNameProvider[Int]
    //	    val vertexNameProvider = new org.jgrapht.ext.VertexNameProvider[Int] {
    //				override def getVertexName(v: Int): String = {
    //					if (v == source1) {
    //						return v.toString + "\" style=filled color=\"#0000ff"
    //					} else if (v == source2) {
    //						return v.toString + "\" style=filled color=\"#ff0000"
    //					} else if (v == dest2) {
    //						return v.toString + "\" style=filled color=\"#00ff00"
    //					} else {
    //						return v.toString
    //					}
    //				}
    //	  	}
    //			val exporter = new org.jgrapht.ext.DOTExporter[Int, ReebEdge](vertexIdProvider, vertexNameProvider, null)
    //	  	val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/t.dot")))
    //	  	exporter.export(bw, SimplifiedReebGraph.this)
    //	  	bw.flush
    //	  	bw.close
    //			
    //			println("Huge error2!")
    //			System.exit(0)
    //		}

    override def cancel: Option[Set[ReebEdge]] = {
      // Construct two paths through the Reeb graph.  The first path will connect extremum to the saddle.  The second path will connect
      // 'killedBy' to the saddle.  Note that the Reeb graph (as it is implemented here) is a directed graph with all edges oriented
      // in the direction of increasing function value.
    	val g = if (compareVertices(extremum, saddle) < 0) SimplifiedReebGraph.this else new EdgeReversedGraph(SimplifiedReebGraph.this)
    	
      val (source1, dest1) = (extremum, saddle)//if (compareVertices(extremum, saddle) < 0) (extremum, saddle) else (saddle, extremum)
      val path1_test = DijkstraShortestPath.findPathBetween(g, source1, dest1)
      
      if (path1_test == null) {
      	throw new RuntimeException
       /* import java.awt.Color
        DebugUtils.getSingleton.saveDebugImage("noPath1.png",
          (g2d: java.awt.Graphics2D) => {
            g2d.setColor(new Color(Color.yellow.getRed, Color.yellow.getGreen, Color.yellow.getBlue, 80))
            for (e <- edgeSet) {
              val p1 = DebugUtils.getSingleton.getImgCoords(e.v1, 0, 1)
              val p2 = DebugUtils.getSingleton.getImgCoords(e.v2, 0, 1)
              g2d.drawLine(p1.x, p1.y, p2.x, p2.y)
            }
            val p1 = DebugUtils.getSingleton.getImgCoords(source1, 0, 1)
            g2d.setColor(Color.red)
            g2d.fillRect(p1.x - 1, p1.y - 1, 3, 3)

            val p2 = DebugUtils.getSingleton.getImgCoords(dest1, 0, 1)
            g2d.setColor(Color.green)
            g2d.fillRect(p2.x - 1, p2.y - 1, 3, 3)
          })
        val vertexIdProvider = new org.jgrapht.ext.IntegerNameProvider[Int]
        val vertexNameProvider = new org.jgrapht.ext.VertexNameProvider[Int] {
          override def getVertexName(v: Int): String = {
            if (v == extremum) {
              return v.toString + "\" style=filled color=\"#0000ff"
            } else if (v == saddle) {
              return v.toString + "\" style=filled color=\"#00ff00"
            } else if (v == killedBy) {
              return v.toString + "\" style=filled color=\"#ff0000"
            } else {
              return v.toString
            }
          }
        }
        val exporter = new org.jgrapht.ext.DOTExporter[Int, ReebEdge](vertexIdProvider, vertexNameProvider, null)
        val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/t.dot")))
        exporter.export(bw, SimplifiedReebGraph.this)
        bw.flush
        bw.close*/
        return None
      }
      val path1 = path1_test.toList
      
      val pathDebug = DijkstraShortestPath.findPathBetween(g, 245882, 143909)
      if (pathDebug == null) {
      	println("Scotty")
      	
      }
      
      val pathDebug2 = DijkstraShortestPath.findPathBetween(g, 143909, 245882)
      if (pathDebug2 == null) {
      	println("Scotty 2")
      }

      val (source2, dest2) = (killedBy, saddle)//if (compareVertices(killedBy, saddle) < 0) (killedBy, saddle) else (saddle, killedBy)
      val path2_test = DijkstraShortestPath.findPathBetween(g, source2, dest2)
      if (path2_test == null) {
      	val exporter = new GraphMLExporter[Int, ReebEdge]
      	 val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/t.gml")))
      	exporter.export(bw, SimplifiedReebGraph.this)
      	bw.flush
      	bw.close
      	throw new RuntimeException
        /*import java.awt.Color
        DebugUtils.getSingleton.saveDebugImage("noPath2.png",
          (g2d: java.awt.Graphics2D) => {
            g2d.setColor(new Color(Color.yellow.getRed, Color.yellow.getGreen, Color.yellow.getBlue, 80))
            for (e <- edgeSet) {
              val p1 = DebugUtils.getSingleton.getImgCoords(e.v1, 0, 1)
              val p2 = DebugUtils.getSingleton.getImgCoords(e.v2, 0, 1)
              g2d.drawLine(p1.x, p1.y, p2.x, p2.y)
            }
            val p1 = DebugUtils.getSingleton.getImgCoords(source2, 0, 1)
            g2d.setColor(Color.red)
            g2d.fillRect(p1.x - 1, p1.y - 1, 3, 3)

            val p2 = DebugUtils.getSingleton.getImgCoords(dest2, 0, 1)
            g2d.setColor(Color.green)
            g2d.fillRect(p2.x - 1, p2.y - 1, 3, 3)
          })
         
        val vertexIdProvider = new org.jgrapht.ext.IntegerNameProvider[Int]
        val vertexNameProvider = new org.jgrapht.ext.VertexNameProvider[Int] {
          override def getVertexName(v: Int): String = {
            if (v == extremum) {
              return v.toString + "\" style=filled color=\"#0000ff"
            } else if (v == saddle) {
              return v.toString + "\" style=filled color=\"#00ff00"
            } else if (v == killedBy) {
              return v.toString + "\" style=filled color=\"#ff0000"
            } else {
              return v.toString
            }
          }
        }
        val exporter = new org.jgrapht.ext.DOTExporter[Int, ReebEdge](vertexIdProvider, null, null)
        val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/t.dot")))
        exporter.export(bw, SimplifiedReebGraph.this)
        bw.flush
        bw.close
        System.exit(0)
        */
        return None
      }
      val path2 = path2_test.toList
      
      // Delete any edges whose function values are in the interval [source1, dest1], and accumulate the list of vertices falling within this interval.
//      def withinInterval(v: Int): Boolean = (v == source1) || (v == dest1) || (compareVertices(source1, v) < 0 && compareVertices(v, dest1) < 0)
//      val edgesToRemove = (path1 ++ path2).filter(edge => withinInterval(edge.v1) || withinInterval(edge.v2))
//      edgesToRemove.forall(removeEdge(_))
			
      
      // TODO we could be smarter about this, but for now, this will work.
      val edgesToRemove = (path1 ++ path2)
      edgesToRemove.forall(removeEdge(_))
      
      // Now construct a new linear path
      val newPathVertices = edgesToRemove.flatMap(e => List(e.v1, e.v2)).distinct.sortWith(compareVertices(_, _) < 0)
      val addedEdges = for ((i, j) <- newPathVertices.zip(newPathVertices.drop(1))) yield {
        val newEdge = new ReebEdge
        addEdge(i, j, newEdge)
        newEdge
      }

      return Some(addedEdges.toSet)
    }
  }
}

object SimplifiedReebGraph {
	val outFile = new BufferedWriter(new FileWriter(new File("/home/harveywi/loopsKilled.csv")))
  def apply(augRG: AugmentedReebGraph, thresh: Float, getFuncVal: Int => Float): SimplifiedReebGraph = {
    println("Killing loops")
    val simpRG = killSmallLoops(thresh, augRG, getFuncVal, augRG.vertComparator.compare(_, _))
    simpRG
  }
	
	override def finalize = {
		println("Finalizing")
		outFile.flush
		outFile.close
	}

  def simplifyH(
    tau: Float,
    H: DirectedWeightedMultigraph[Int, DefaultWeightedEdge],
    filtration: Seq[Int],
    fakeOfs: Int,
    dummyMin: Int,
    dummyMax: Int,
    getFuncVal: Int => Float,
    compareVertices: (Int, Int) => Int): Unit = {

    def isReal(i: Int) = i < fakeOfs
    def isFake(i: Int) = !isReal(i)
    def toReal(i: Int) = if (isReal(i)) i else i - fakeOfs
    def toFake(i: Int) = if (isFake(i)) i else i + fakeOfs

    def getEdgePair(e: DefaultWeightedEdge): Option[(DefaultWeightedEdge, DefaultWeightedEdge)] = {
      val i = H.getEdgeSource(e)
      val j = H.getEdgeTarget(e)
      if (isReal(i) && isReal(j)) {
        return Some((e, H.getEdge(toFake(j), toFake(i))))
      } else if (isFake(i) && isFake(j)) {
        return Some((H.getEdge(toReal(j), toReal(i)), e))
      } else {
        None
      }
    }

    def removeEdgePair(e: DefaultWeightedEdge): Unit = {
      getEdgePair(e) match {
        case Some((e1, e2)) => {
          H.removeEdge(e1)
          H.removeEdge(e2)
        }
        case None => { /*Do nothing*/ }
      }
    }
    
    def killAllLoops(omega: Int, outFile: BufferedWriter): Boolean = {
    	val incomingEdges = H.incomingEdgesOf(omega).filter(e => isReal(H.getEdgeSource(e))).toArray
    	
      var i = 0
      while (i < incomingEdges.size) {
        val e1 = incomingEdges(i)
        val x = H.getEdgeSource(e1)

        var j = i + 1
        while (j < incomingEdges.size) {
          val e2 = incomingEdges(j)
          val y = H.getEdgeSource(e2)

          if (x == y) {
            //removeEdgePair(e2)
          } else {
            val yPrime = toFake(y)

            val maxPathLength = 2 * tau - H.getEdgeWeight(e1) - H.getEdgeWeight(e2)
            val dijkstra = new DijkstraShortestPath(H, yPrime, x)
            val path = dijkstra.getPath
            if (path != null) {
              //assert((path.getWeight + H.getEdgeWeight(e1) + H.getEdgeWeight(e2)) / 2.0 <= tau)
              val (downPath, upPath) = path.getEdgeList.partition(e => isFake(H.getEdgeSource(e)))
              val weightDown = H.getEdgeWeight(e2) + downPath.map(H.getEdgeWeight(_)).sum
              val weightUp = upPath.map(H.getEdgeWeight(_)).sum + H.getEdgeWeight(e1)
              //							val edgeList = path.getEdgeList.toList
              //							val vertList = H.getEdgeSource(edgeList.head) :: edgeList.tail.map(H.getEdgeTarget(_))

              val loopHeight = math.min(weightDown, weightUp)
//              if (weightDown != weightUp) {
//                val z = 3
//              }
              
              outFile.write(loopHeight + "\n")

              if (loopHeight < tau) {
                // Remove the loop's constituent edges from G and G' and accumulate the list of vertices participating
                // in the loop
                val pathVerts = new scala.collection.mutable.ListBuffer[Int]
                pathVerts += x
                pathVerts += omega
                path.getEdgeList.foreach(e => pathVerts += toReal(H.getEdgeSource(e)))

                removeEdgePair(e1)
                removeEdgePair(e2)
                path.getEdgeList.foreach { e =>
                  removeEdgePair(e)
                }

                // Now connect the affected vertices linearly
                val vertsSorted = pathVerts.distinct.sortWith(compareVertices(_, _) < 0).toArray
                (0 until vertsSorted.size - 1).foreach { i =>
                  val v1 = vertsSorted(i)
                  val v1Fake = toFake(v1)
                  val v2 = vertsSorted(i + 1)
                  val v2Fake = toFake(v2)
                  //val edgeWeight = math.abs(getFuncVal(v1) - getFuncVal(v2))
                  val edgeWeight = if (v1 == dummyMin || v1 == dummyMax || v2 == dummyMin || v2 == dummyMax) {
                    0f
                  } else {
                    math.abs(getFuncVal(v1) - getFuncVal(v2))
                  }

                  val eReal = H.addEdge(v1, v2)
                  H.setEdgeWeight(eReal, edgeWeight)

                  val eFake = H.addEdge(toFake(v2), toFake(v1))
                  H.setEdgeWeight(eFake, edgeWeight)
                }
                return false
              }
            }
          }

          j += 1
        }
        i += 1
      }
    	
    	return true
    }
    
    for (omega <- filtration if H.inDegreeOf(omega) > 2) {
    	while (!killAllLoops(omega, outFile)) {}
    }
    
//    for (omega <- filtration if H.inDegreeOf(omega) > 2) {
//      // Kill any loops whose hi-point is omega.  We need to check all pairs of immediate ancestors of omega
//      // (excluding the immediate ancestor that is in subgraph G', which has a negative index).
//      var incomingEdges = H.incomingEdgesOf(omega).filter(e => isReal(H.getEdgeSource(e))).toArray
//
//      var i = 0
//      while (i < incomingEdges.size) {
//        val e1 = incomingEdges(i)
//        val x = H.getEdgeSource(e1)
//
//        var j = i + 1
//        while (j < incomingEdges.size) {
//          val e2 = incomingEdges(j)
//          val y = H.getEdgeSource(e2)
//
//          if (x == y) {
//            //removeEdgePair(e2)
//          } else {
//            val yPrime = toFake(y)
//
//            val maxPathLength = 2 * tau - H.getEdgeWeight(e1) - H.getEdgeWeight(e2)
//            val dijkstra = new DijkstraShortestPath(H, yPrime, x)
//            val path = dijkstra.getPath
//            if (path != null) {
//              //assert((path.getWeight + H.getEdgeWeight(e1) + H.getEdgeWeight(e2)) / 2.0 <= tau)
//              val (downPath, upPath) = path.getEdgeList.partition(e => isFake(H.getEdgeSource(e)))
//              val weightDown = H.getEdgeWeight(e2) + downPath.map(H.getEdgeWeight(_)).sum
//              val weightUp = upPath.map(H.getEdgeWeight(_)).sum + H.getEdgeWeight(e1)
//              //							val edgeList = path.getEdgeList.toList
//              //							val vertList = H.getEdgeSource(edgeList.head) :: edgeList.tail.map(H.getEdgeTarget(_))
//
//              val loopHeight = math.min(weightDown, weightUp)
//              if (weightDown != weightUp) {
//                val z = 3
//              }
//
//              if (loopHeight < tau) {
//              	j = incomingEdges.size
//              	i = 0
//                // Remove the loop's constituent edges from G and G' and accumulate the list of vertices participating
//                // in the loop
//                val pathVerts = new scala.collection.mutable.ListBuffer[Int]
//                pathVerts += x
//                pathVerts += omega
//                path.getEdgeList.foreach(e => pathVerts += toReal(H.getEdgeSource(e)))
//
//                removeEdgePair(e1)
//                removeEdgePair(e2)
//                path.getEdgeList.foreach { e =>
//                  removeEdgePair(e)
//                }
//
//                // Now connect the affected vertices linearly
//                val vertsSorted = pathVerts.distinct.sortWith(compareVertices(_, _) < 0).toArray
//                (0 until vertsSorted.size - 1).foreach { i =>
//                  val v1 = vertsSorted(i)
//                  val v1Fake = toFake(v1)
//                  val v2 = vertsSorted(i + 1)
//                  val v2Fake = toFake(v2)
//                  //val edgeWeight = math.abs(getFuncVal(v1) - getFuncVal(v2))
//                  val edgeWeight = if (v1 == dummyMin || v1 == dummyMax || v2 == dummyMin || v2 == dummyMax) {
//                    0f
//                  } else {
//                    math.abs(getFuncVal(v1) - getFuncVal(v2))
//                  }
//
//                  val eReal = H.addEdge(v1, v2)
//                  H.setEdgeWeight(eReal, edgeWeight)
//
//                  val eFake = H.addEdge(toFake(v2), toFake(v1))
//                  H.setEdgeWeight(eFake, edgeWeight)
//                }
//              }
//            }
//          }
//
//          j += 1
//        }
//        i += 1
//      }
//    }
//
  }

  /**
   * Strategy:
   * 
   * Start with (augmented) Reeb graph G.  Construct graph G' as follows:
   * 	- G' duplicates all vertices of G.
   * 	- G' duplicates all edges of G, but directions are reversed.
   * 	
   * Now link G' up to G as follows to get graph H:
   * 	- For every vertex v' in G', add a zero-weight edge to vertex v in G.
   * 
   * Terminology note:  Vertices in G will be called "primary" vertices, whereas vertices in G' will be called 
   * "secondary" vertices.
   * 
   * To simplify the Reeb graph, we will find any vertex whose 
   * 
   * @param tau
   */
  def killSmallLoops(tau: Float, augRG: AugmentedReebGraph, getFuncVal: Int => Float, compareVerticesIn: (Int, Int) => Int): SimplifiedReebGraph = {

    // Add a dummy global max and a dummy global min.  The dummy global min will feed into all
    // the local minima, and all the local maxima will feed into the dummy global max.
    val localMinima = augRG.vertexSet.filter(augRG.smallerNeighborsOf(_).size == 0)
    val localMaxima = augRG.vertexSet.filter(augRG.largerNeighborsOf(_).size == 0)
    val dummyMin = augRG.vertexSet.size
    val dummyMax = augRG.vertexSet.size + 1
    augRG.addVertex(dummyMin)
    augRG.addVertex(dummyMax)
    localMinima.foreach { min =>
      augRG.addEdge(dummyMin, min, new ReebEdge)
    }
    localMaxima.foreach { max =>
      augRG.addEdge(max, dummyMax, new ReebEdge)
    }

    val fakeOfs = augRG.vertexSet.size
    val H = new DirectedWeightedMultigraph[Int, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    augRG.vertexSet.foreach { v =>
      H.addVertex(v)
      H.addVertex(v + fakeOfs)
    }

    def isReal(i: Int) = i < fakeOfs
    def isFake(i: Int) = !isReal(i)
    def toReal(i: Int) = if (isReal(i)) i else i - fakeOfs
    def toFake(i: Int) = if (isFake(i)) i else i + fakeOfs

    augRG.edgeSet.foreach { reebEdge =>
      val i = reebEdge.v1
      val j = reebEdge.v2
      val weight = if (i == dummyMin || j == dummyMax) {
        0
      } else {
        assert(i != dummyMin && j != dummyMin && i != dummyMax && j != dummyMax)
        math.abs(getFuncVal(i) - getFuncVal(j)).toFloat
      }
      val e1 = H.addEdge(i, j)
      H.setEdgeWeight(e1, weight)
      val e2 = H.addEdge(toFake(j), toFake(i))
      H.setEdgeWeight(e2, weight)
    }

    augRG.vertexSet.foreach { v =>
      val e = H.addEdge(toFake(v), v)
      H.setEdgeWeight(e, 0)
    }

    def compareVerticesAsc(v1: Int, v2: Int): Int = {
      if (v1 == dummyMin || v2 == dummyMax) {
        return -1
      } else if (v1 == dummyMax || v2 == dummyMin) {
        return 1
      } else {
        return compareVerticesIn(v1, v2)
      }
    }

    def compareVerticesDesc(v1: Int, v2: Int): Int = -compareVerticesAsc(v1, v2)

    val filtrationAsc = augRG.vertexSet.toSeq.sortWith(compareVerticesAsc(_, _) < 0).dropRight(1)
    simplifyH(tau, H, filtrationAsc, fakeOfs, dummyMin, dummyMax, getFuncVal, compareVerticesAsc)

    def reverseEdges() = {
      val edgesToReverse = H.edgeSet.filter { e =>
        val v1 = H.getEdgeSource(e)
        val v2 = H.getEdgeTarget(e)
        (isReal(v1) && isReal(v2)) || (isFake(v1) && isFake(v2))
      }

      edgesToReverse.foreach { e =>
        val v1 = H.getEdgeSource(e)
        val v2 = H.getEdgeTarget(e)
        val w = H.getEdgeWeight(e)
        H.removeEdge(e)
        val newEdge = H.addEdge(v2, v1)
        H.setEdgeWeight(newEdge, w)
      }
    }

    // Now reverse the (real, real) edges and (fake, fake) edges of H and redo the simplification using a reversed filtration
    reverseEdges()

    val filtrationDesc = augRG.vertexSet.toSeq.sortWith(compareVerticesDesc(_, _) < 0).dropRight(1)
    simplifyH(tau, H, filtrationDesc, fakeOfs, dummyMin, dummyMax, getFuncVal, compareVerticesDesc)

    // Reverse the edges one last time so that all the (real-real) edges' directions coincide with increasing function value
    reverseEdges()
    //simplifyH(tau, H, filtrationAsc, fakeOfs, dummyMin, dummyMax, getFuncVal, compareVerticesAsc)
/*
    reverseEdges()
    simplifyH(tau, H, filtrationDesc, fakeOfs, dummyMin, dummyMax, getFuncVal, compareVerticesDesc)
    reverseEdges()
    simplifyH(tau, H, filtrationAsc, fakeOfs, dummyMin, dummyMax, getFuncVal, compareVerticesAsc)
*/
    // Now we will rebuild the augmented Reeb graph using the structure of G, but ignoring all the edges
    // that are connected to the dummy global min and the dummy global max
    val simpRG = new SimplifiedReebGraph(augRG.vertComparator, getFuncVal)
    augRG.vertexSet.foreach { v =>
      if (v != dummyMin && v != dummyMax)
        simpRG.addVertex(v)
    }
    H.edgeSet.foreach { e =>
      val v1 = H.getEdgeSource(e)
      val v2 = H.getEdgeTarget(e)
      if (isReal(v1) && isReal(v2) && v1 != dummyMin && v2 != dummyMax) {
        simpRG.addEdge(v1, v2, new ReebEdge)
      }
    }

    augRG.removeVertex(dummyMin)
    augRG.removeVertex(dummyMax)

    return simpRG
  }

  def main(args: Array[String]): Unit = {
    val augRG = new AugmentedReebGraph(new Comparator[Int] { def compare(i: Int, j: Int) = i.compare(j) })

    (0 to 7).foreach(augRG.addVertex)
    val edgePairs = List(
      (0, 1),
      (1, 2),
      (1, 3),
      (2, 4),
      (2, 5),
      (3, 4),
      (3, 5),
      (4, 6),
      (5, 6),
      (6, 7))

    edgePairs.foreach { case (v1, v2) => augRG.addEdge(v1, v2, new ReebEdge) }

    val rgSimp = SimplifiedReebGraph.killSmallLoops(1000, augRG, (x: Int) => x, augRG.vertComparator.compare(_, _))

    //		(0 to 13).foreach(augRG.addVertex)
    //		val edgePairs = List(
    //				(0,2),
    //				(1,3),
    //				(2,3),
    //				(2,8),
    //				(3,4),
    //				(4,5),
    //				(4,7),
    //				(5,6),
    //				(5,8),
    //				(6,7),
    //				(6,10),
    //				(7,9),
    //				(8,11),
    //				(9,10),
    //				(9,11),
    //				(10,12),
    //				(11,13)
    //		)
    //		edgePairs.foreach{case (v1, v2) => augRG.addEdge(v1, v2, new ReebEdge)}
    //		val standardPairs = augRG.getStandardPersistencePairs
    //		
    //		println("Standard persistence pairs: " + standardPairs.mkString(","))
    //		
    //		val allPairs = (standardPairs).sortWith(_ < _)
    //		val supersetPairs = allPairs.toSet
    //		for (pair <- allPairs) {
    //			println("Canceling " + pair)
    //			pair.cancel
    //			val newStandard = augRG.getStandardPersistencePairs
    //			println("\tNew standard:  " + newStandard.mkString(","))
    //			val newPairs = newStandard
    ////			val newPairs = (augRG.getStandardPersistencePairs ++ augRG.getExtendedPersistencePairsNew())
    ////			println("\tNew persistence pairs size:  " + newPairs.size)
    //			
    //			assert(newPairs.forall(supersetPairs.contains))
    //		}
    //		println("Done!")
    //		
    //		//pairs.foreach(println)

  }
  
  def simplifyStandardPersistence(rgAug: AugmentedReebGraph, getFuncVal: Int => Float, tau: Float): SimplifiedReebGraph = {
  	val rgSimp = new SimplifiedReebGraph(rgAug.vertComparator, getFuncVal)
  	rgAug.vertexSet.foreach(rgSimp.addVertex(_))
   	rgAug.edgeSet.foreach(e => rgSimp.addEdge(e.v1, e.v2, new ReebEdge))
  	val allPairs = rgSimp.getStandardPersistencePairs
  	
  	allPairs.filter(_.persistence < tau).foreach(pair => {
  		if (rgSimp.compareVertices(pair.killedBy, pair.extremum) < 0) {
  			rgSimp.addEdge(pair.killedBy, pair.extremum, new ReebEdge)
  		} else {
  			rgSimp.addEdge(pair.extremum, pair.killedBy, new ReebEdge)
  		}
  	})
  	
  	val newPairs = rgSimp.getStandardPersistencePairs
  	println()
  	println("oldPairs:  " + allPairs.size)
  	println("newPairs:  " + newPairs.size)
  	assert(newPairs.forall(_.persistence >= tau))
  	System.exit(0)
  	
  	// What happens if we build a graph consisting 
  	
  	null
  }
  
  def simplifyStandardPersistenceOld2(rgAug: AugmentedReebGraph, getFuncVal: Int => Float, tau: Float): SimplifiedReebGraph = {
/*
	val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/ct.graph")))
	bw.write("""
Graph
{
"Contour tree";
"blah";
%d; %d; 0; 0;
""".format(rgAug.vertexSet.size, rgAug.edgeSet.size))
	val edgeStrings = rgAug.edgeSet.map(e => {
		"{%d;%d;}".format(e.v1, e.v2)
	})
	
	bw.write(edgeStrings.mkString("[\n", ",\n", "];\n"))
	bw.write("""
    ;                   ## path list
    ; ; ;               ## enum-def, attr-def, qualifer lists
    ; ; ; ;             ## visualization hints
    ; ; ; ; ;           ## interface hints
}""")
		bw.flush
		bw.close
  	
  	System.exit(0)
*/
  	val rgSimp = new SimplifiedReebGraph(rgAug.vertComparator, getFuncVal)
  	rgAug.vertexSet.foreach(rgSimp.addVertex(_))
   	rgAug.edgeSet.foreach(e => rgSimp.addEdge(e.v1, e.v2, new ReebEdge))
   	
   	val allPairs = rgSimp.getStandardPersistencePairs
   	val (upForks, downForks) = allPairs.partition(pair => rgSimp.largerNeighborsOf(pair.extremum).isEmpty)
   	println("Up forks: " + upForks.size)
   	println("Down forks:  " + downForks.size)
   	
   	// Try canceling all the down forks first, then all the up forks.
   	val downForksSorted = downForks.sortWith((pair1, pair2) => {
   		if (pair1.saddle == pair2.saddle) {
   			rgSimp.compareVertices(pair2.extremum, pair1.extremum) < 0
   		} else {
   			pair1.persistence < pair2.persistence
   		}
   	})
   	
   	var n = downForksSorted.size
  	println("Euler simple:  " + (rgAug.vertexSet.size - rgAug.edgeSet.size))
  	
  	downForksSorted.zipWithIndex.foreach{case (pair, i) => {
  		println("Canceling pair " + (i+1) + " of " + n)
  		println("Euler before:  " + (rgSimp.vertexSet.size - rgSimp.edgeSet.size))
  		println("\tPersistence:  " + pair.persistence)
  		pair.cancel
  		println("Euler after:  " + (rgSimp.vertexSet.size - rgSimp.edgeSet.size))
  	}}
  	
//  	val upForksSorted = upForks.sortWith((pair1, pair2) => {
//  		if (pair1.saddle == pair2.saddle) {
//  			rgSimp.compareVertices(pair1.extremum, pair2.extremum) < 0
//  		} else {
//  			pair1.persistence < pair2.persistence
//  		}
//  	})
//  	
//  	var n = upForksSorted.size
//  	println("Euler simple:  " + (rgAug.vertexSet.size - rgAug.edgeSet.size))
//  	
//  	upForksSorted.zipWithIndex.foreach{case (pair, i) => {
//  		println("Canceling pair " + (i+1) + " of " + n)
//  		println("Euler before:  " + (rgSimp.vertexSet.size - rgSimp.edgeSet.size))
//  		println("\tPersistence:  " + pair.persistence)
//  		pair.cancel
//  		if ((rgSimp.vertexSet.size - rgSimp.edgeSet.size) != 1) {
//  			val z = 3
//  		}
//  		println("Euler after:  " + (rgSimp.vertexSet.size - rgSimp.edgeSet.size))
//  	}}
  	
  	println("Made it!")

   	System.exit(0)
  	null
  }
  
  def simplifyStandardPersistenceOld(rgAug: AugmentedReebGraph, getFuncVal: Int => Float, tau: Float): SimplifiedReebGraph = {
  	val rgSimp = new SimplifiedReebGraph(rgAug.vertComparator, getFuncVal)
  	rgAug.vertexSet.foreach(rgSimp.addVertex(_))
  	rgAug.edgeSet.foreach(e => rgSimp.addEdge(e.v1, e.v2, new ReebEdge))
  	
  	val numMinima = rgAug.vertexSet.count(v => rgAug.smallerNeighborsOf(v).isEmpty)
  	val numMaxima = rgAug.vertexSet.count(v => rgAug.largerNeighborsOf(v).isEmpty)
  	println("Num minima:  " + numMinima)
  	println("Num maxima:  " + numMaxima)
  	
  	val allPairs = rgSimp.getStandardPersistencePairs.sortWith((pair1, pair2) => {
  		if (pair1.persistence == pair2.persistence) {
  			if (pair1.saddle == pair2.saddle) {
  				if (rgSimp.smallerNeighborsOf(pair1.extremum).isEmpty && rgSimp.smallerNeighborsOf(pair2.extremum).isEmpty) {
  					// Compare two down-forks sharing the same saddle
  					// The pair whose extremum has the higher function value will be less persistent
  					rgSimp.compareVertices(pair2.extremum, pair1.extremum) < 0
  				} else if (rgSimp.largerNeighborsOf(pair1.extremum).isEmpty && rgSimp.largerNeighborsOf(pair2.extremum).isEmpty) {
  					// Compare two up-forks sharing the same saddle
  					// The pair whose extremum has the smaller function value will be less persistent
  					rgSimp.compareVertices(pair1.extremum, pair2.extremum) < 0
  				} else {
  					// One of them is an up-fork and one of them is a down-fork - who cares how we sort them
  					false
  				}
  			} else {
  				false
  			}
  		} else {
  			pair1.persistence < pair2.persistence
  		}
  	})
  	val pairs = allPairs.filter(_.persistence < tau)
  	val n = pairs.size
  	println("Euler simple:  " + (rgAug.vertexSet.size - rgAug.edgeSet.size))
  	
  	pairs.zipWithIndex.foreach{case (pair, i) => {
  		println("Canceling pair " + (i+1) + " of " + n)
  		println("Euler before:  " + (rgSimp.vertexSet.size - rgSimp.edgeSet.size))
  		println("\tPersistence:  " + pair.persistence)
  		pair.cancel
  		println("Euler after:  " + (rgSimp.vertexSet.size - rgSimp.edgeSet.size))
  	}}
  	
  	// Now contract the simplified graph
  	val numContracted = rgSimp.vertexSet.count(rgSimp.contract(_))
  	println("All pairs size:  " + allPairs.size)
  	println("thresh pairs size:  " + pairs.size)
  	println("Num contracted:  " + numContracted)
  	println("Edges left: " + rgSimp.edgeSet.size)
  	
  	return rgSimp
  }
}