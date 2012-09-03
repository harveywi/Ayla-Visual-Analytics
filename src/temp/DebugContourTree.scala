package temp

import edu.osu.compgeom.util.UnionFind
import edu.osu.compgeom.util.IO._
import java.io._
import edu.osu.compgeom.ayla.AylaCollaborationProject
import org.jgrapht.graph._
import org.jgrapht.alg._
import edu.osu.compgeom.topology.ScalarFunction
import edu.osu.compgeom.ct.ContourTree
import edu.osu.compgeom.ct.ContourTreeNode

object DebugContourTree {
  def main(args: Array[String]): Unit = {

//    def go = {
//      val (v1, v2, v3, v4, v5, v6) = (0, 1, 2, 3, 4, 5)
//      val verts = Array(v1, v2, v3, v4, v5, v6).map(i => Array(i.toFloat))
//      val faces = Array(
//        (v1, v2), (v1, v4), (v2, v3), (v2, v5), (v3, v6), (v4, v5), (v5, v6)).map(t => Array(t._1, t._2))
//      val funcVals = Array(10, 40, 70, 60, 80, 50).map(_.toFloat)
//      val sf = new ScalarFunction(verts, faces, funcVals)
//
//      val ct = ContourTree(sf)
//      ct.nodesAugmented.foreach(n => {
//        println("Vertex:  v%d".format(n.vertex + 1))
//        println("Upper neighbors:  " + n.parents.map(p => "v" + (p.vertex + 1)).mkString("(", " ", ")"))
//      })
//
//      val g2 = new SimpleGraph[ContourTreeNode, DefaultEdge](classOf[DefaultEdge])
//      ct.nodesAugmented.foreach(g2.addVertex)
//      ct.nodesAugmented.foreach { n =>
//        n.parents.foreach(p => g2.addEdge(n, p))
//      }
//      val ci = new ConnectivityInspector(g2)
//      println("Connected?  " + ci.isGraphConnected())
//      println("Betti zero:  " + ct.scalarFunction.bettiZero)
//      println("num cc:  " + ci.connectedSets.size)
//
//    }
//    go
//    System.exit(0)

    val sf = withObjectInputStream(new File("/home/harveywi/research/ScalaCrystals/Survivin_252996/collab_projects/20000_k15_low_e.dat")) { ois =>
      ois.readObject().asInstanceOf[AylaCollaborationProject].sf
    }

    val g = new SimpleGraph[Int, DefaultEdge](classOf[DefaultEdge])
    sf.vertices.indices.foreach(g.addVertex)
    sf.faces.foreach(e => g.addEdge(e(0), e(1)))

    // 104 to 138

    Iterator.continually(31699).foreach { source =>
      val bf = new BellmanFordShortestPath(g, source)
      val sampledToUnsampled = sf.vertices.indices.filter { i =>
        i == source || bf.getCost(i) < 3
      }.toArray
      //      val sampledToUnsampled = Array(587,1167,1397,2378,2644,3783,4152,4157,4187,4250,4398,4665,4788,5311,6221,6421,6627,6717,6881,7221,7453,8056,8109,8558,8561,9596,9944,10190,11405,12440,13775,15854,16628,17151,18497,21102,21445,22346,22842,22978,23542,23611,24324,24854,26481,26677,28185,28208,29271,29914,29970,31274,31699,31917,32626,33604,33771,34065,34467,34881,36588,37084,38186,38715,40208,43685)
      println("size:  " + sampledToUnsampled.size)

      val unsampledToSampled = sampledToUnsampled.zipWithIndex.toMap
      val sampleSet = sampledToUnsampled.toSet

      val sampledEdges = sf.faces.filter(e => sampleSet.contains(e(0)) && sampleSet.contains(e(1))).map(e => e.map(unsampledToSampled))
      val sampledFuncVals = sampledToUnsampled.map(sf.getFuncVal)

      println("f:  " + sampledFuncVals.size)

      val sf2 = new ScalarFunction(sampledToUnsampled.map(sf.vertices), sampledEdges, sampledFuncVals)

      val ct = ContourTree(sf2)
      val g2 = new SimpleGraph[ContourTreeNode, DefaultEdge](classOf[DefaultEdge])
      ct.nodesAugmented.foreach(g2.addVertex)
      ct.nodesAugmented.foreach { n =>
        n.parents.foreach(p => g2.addEdge(n, p))
      }
      val ci = new ConnectivityInspector(g2)
      println("Connected?  " + ci.isGraphConnected())
      println("Betti zero:  " + ct.scalarFunction.bettiZero)
      println("num cc:  " + ci.connectedSets.size)
      if (ci.connectedSets.size != 1) {
        println("***FOUND!  " + source)
        val maxSet = (0 until 3).map(ci.connectedSets.get(_)).maxBy(_.size())
        (0 until 3).foreach { i =>
          val set = ci.connectedSets.get(i)
          if (set != maxSet) {
            println(set)
          }
        }

        System.exit(0)
      }
    }

    //	  val uf = new UnionFind(252996)
    //	  
    //	  val edges = withBufferedReader(new File("/dev/shm/aug.dot")){br =>
    //	    Iterator.continually(br.readLine).takeWhile(_ != null).drop(1).takeWhile(s => !s.contains('}')).toArray
    //	  }
    //	  
    //	  val pat = " -> ".r
    //	  edges.foreach{e =>
    //	    val es = pat.split(e).map(_.toInt)
    //	    if (es.contains(21028) || es.contains(20112))
    //	      println(e)
    //	    uf.union(es(0), es(1))
    //	  }
    //	  
    //	  val bettiZero = (0 until 252996).map(uf.find).toSet.size
    //	  println("Betti zero:  " + bettiZero)
  }
}