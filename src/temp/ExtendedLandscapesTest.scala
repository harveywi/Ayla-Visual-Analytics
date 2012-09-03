package temp

import edu.osu.compgeom.dataset.CachedDataset
import java.io._
import edu.osu.compgeom.topology.ScalarFunction
import scala.swing._
import edu.osu.compgeom.ct.ContourTree

import java.awt.image.BufferedImage
import java.awt.Color

import org.jgrapht.graph._
import org.jgrapht.traverse._

import scala.collection.JavaConversions._

object ExtendedLandscapesTest {
	/*
  def main(args: Array[String]): Unit = {
    edu.osu.compgeom.util.Debug.debugMode = true
    val k = 17
    val dataset = new CachedDataset(new File("/home/harveywi/research/ScalaCrystals/Survivin_252996"))
    val lines = scala.io.Source.fromFile(new File("/home/harveywi/research/ScalaCrystals/Survivin_252996/scalar_functions/potential_energy.txt")).getLines
    val morseFuncVals = lines.drop(1).map(_.toFloat).toArray
    
    println("Extracting 1-skeleton")
		val edgesWithDupes = (0 until dataset.neighbors.size).flatMap(i => 
			dataset.neighbors(i).take(17).map(j => Tuple2(math.min(i, j), math.max(i, j)))
		).toArray
		
		println("Removing duplicate edges")
		val edges = edgesWithDupes.distinct.flatMap(e => if (e._1 != e._2) Some(Array(e._1, e._2)) else None).toArray
    
		val vertices = dataset.pcaPoints.toArray
		
		println("Constructing Morse function")
		val morseFunction = new ScalarFunction(vertices, edges, morseFuncVals)
    
    println("Calculating crystals...")
    val (crystals, _) = morseFunction.getMorseCrystals(60f)
    println("Done calculating crystals!")
    
    val minima = crystals.map(_.min).toSet
    val maxima = crystals.map(_.max).toSet
    
    println("Getting contour tree (for the purpose of getting join/split trees)")
    
    val ct = ContourTree(morseFunction, (x: Array[Int]) => x.size)
    
    var brJoin = new BufferedReader(new FileReader(new File("/dev/shm/join.dot")))
    val jt = new SimpleDirectedGraph[Int, DefaultEdge](classOf[DefaultEdge])
    val pat = " -> ".r
    
    
    val jtVerts = new scala.collection.mutable.HashSet[Int]
    while (brJoin.ready) {
      val s = pat.split(brJoin.readLine)
      if (s.size == 2) {
        val v1 = s.head.toInt
        val v2 = s.last.toInt
        jtVerts += v1
        jtVerts += v2
      }
    }
    brJoin = new BufferedReader(new FileReader(new File("/dev/shm/join.dot")))
    jtVerts.foreach(jt.addVertex)
    
    while (brJoin.ready) {
      val s = pat.split(brJoin.readLine)
      if (s.size == 2) {
        val v1 = s.head.toInt
        val v2 = s.last.toInt
        jt.addEdge(v2, v1)
      }
    }
    brJoin.close
    
    var brSplit = new BufferedReader(new FileReader(new File("/dev/shm/split.dot")))
    val st = new SimpleDirectedGraph[Int, DefaultEdge](classOf[DefaultEdge])
    val stVerts = new scala.collection.mutable.HashSet[Int]
    while (brSplit.ready) {
      val s = pat.split(brSplit.readLine)
      if (s.size == 2) {
        val v1 = s.head.toInt
        val v2 = s.last.toInt
        stVerts += v1
        stVerts += v2
      }
    }
    brSplit = new BufferedReader(new FileReader(new File("/dev/shm/split.dot")))
    stVerts.foreach(st.addVertex)
    
    while (brSplit.ready) {
      val s = pat.split(brSplit.readLine)
      if (s.size == 2) {
        val v1 = s.head.toInt
        val v2 = s.last.toInt
        st.addEdge(v2, v1)
      }
    }
    
    val startJT = jt.vertexSet.find(v => jt.incomingEdgesOf(v).size == 0).get
    val jtIt = new DepthFirstIterator(jt, startJT)
    var numMin = 0
    var numMax = 0
    var minOrdering = new scala.collection.mutable.ArrayBuffer[Int]
    while (jtIt.hasNext) {
      val n = jtIt.next
      if (minima.contains(n)) {
        println("MIN " + n)
        minOrdering += n
        numMin += 1
      }
    }
    
    val startST = st.vertexSet.find(v => st.incomingEdgesOf(v).size == 0).get
    val stIt = new DepthFirstIterator(st, startST)
    val maxOrdering = new scala.collection.mutable.ArrayBuffer[Int]
    while (stIt.hasNext) {
      val n = stIt.next
      if (maxima.contains(n)) {
        maxOrdering += n
      }
    }
    
    println(minOrdering.size + "\t" + maxOrdering.size)
    println(minima.size + "\t" + maxima.size)
    
    println((minima -- minOrdering.toSet).size)
    println(jt.vertexSet.count(v => jt.incomingEdgesOf(v).size == 0))
    println(st.vertexSet.count(v => st.incomingEdgesOf(v).size == 0))
    
    val minimaToIdx = minOrdering.zipWithIndex.toMap
    val maximaToIdx = maxOrdering.zipWithIndex.toMap
    
    val table = Array.ofDim[Int](minima.size, maxima.size)
    
    crystals.foreach(c => {
      val area = c.pts.size
      if (minimaToIdx.contains(c.min) && maximaToIdx.contains(c.max)) {
        table(minimaToIdx(c.min))(maximaToIdx(c.max)) = 1
      } else {
        println("Warning:")
        if (!minimaToIdx.contains(c.min)) {
          println("\tmin not found")
        }
        if (!maximaToIdx.contains(c.max)) {
          println("\tmax not found")
        }
      }
      
    })
    val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/a.csv")))
    table.foreach(row => bw.write(row.mkString("", ",", "\n")))
    bw.flush
    bw.close
    
  }
  */
}