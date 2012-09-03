package temp

import java.awt.Graphics
import org.jmol.adapter.smarter.SmarterJmolAdapter
import java.awt.Rectangle
import java.awt.Dimension
import org.jmol.api.JmolViewer
import javax.swing.JPanel
import java.awt.geom.Ellipse2D
import scala.swing._
import edu.osu.compgeom.dataset.CachedDataset
import java.io._
import scala.collection.mutable.ArrayBuffer
import javax.vecmath.Point3f
import org.jgrapht.graph._
import org.jgrapht.alg._
import edu.osu.compgeom.dataset.FilePdbStreamProvider

object GetL {

  class NativeContact(val i: Int, val j: Int, val dist: Double)

  def main(args: Array[String]): Unit = {
    val dir = new File("/home/harveywi/research/ScalaCrystals/TrpCage")

    val dataset = new CachedDataset(dir) with FilePdbStreamProvider
    
    val bwL = new BufferedWriter(new FileWriter(new File(dir, "scalar_functions/L_nmr.txt")))
    bwL.write("L (nmr)\n")
    
//    val densityThresh = 7;
//    val lVals = dataset.pdbFiles.get.map(pdbFile => {
//      val backbone = 
//    })
    
    
    
    val lines = scala.io.Source.fromFile(new File(dir, "scalar_functions/potential_energy.txt")).getLines
    val morseFuncVals = lines.drop(1).map(_.toFloat).toArray

    val minIdx = morseFuncVals.view.zipWithIndex.sortWith((t1, t2) => t1._1 < t2._1).head._2

//    val minPDB = dataset.pdbFiles.get(minIdx)
    val minPDB = new File("/home/harveywi/Desktop/trpcage_native.pdb")

    val minBackbone = getCarbonBackbone(minPDB)

    val densityThresh = 7;
    val nativeContacts =
      for (
        i <- 0 until minBackbone.size;
        j <- (i + 1) until minBackbone.size;
        dist = minBackbone(i).distance(minBackbone(j));
        if dist <= densityThresh
      ) yield new NativeContact(i, j, dist)

    val numNativeContacts = nativeContacts.size.toDouble
    println("Num native contacts:  " + numNativeContacts)
    
//    val g = new SimpleGraph[Int, DefaultEdge](classOf[DefaultEdge])
//    minBackbone.indices.foreach(g.addVertex)
//    (0 until minBackbone.size - 1).foreach(i => g.addEdge(i, i+1))
    
    val lVals = dataset.pdbFiles.par.map(pdbFile => {
      val backbone = getCarbonBackbone(pdbFile)
      val g = new SimpleGraph[Int, DefaultEdge](classOf[DefaultEdge])
      backbone.indices.foreach(g.addVertex)
      (0 until backbone.size - 1).foreach(i => g.addEdge(i, i+1))
      nativeContacts.foreach(nc => {
        val dist = backbone(nc.i).distance(backbone(nc.j))
        if (dist <= densityThresh) {
          g.addEdge(nc.i, nc.j)
        }
      })
      
      val fw = new FloydWarshallShortestPaths(g)
//      val graphDistances = backbone.indices.combinations(2).map(pair => fw.shortestDistance(pair.head, pair.last))
      val graphDistances = (0 until backbone.size).flatMap(i => {
        (i+1 until backbone.size).map(j => {
          fw.shortestDistance(i, j)
        })
      })
      val numPairs = (backbone.size * (backbone.size-1)) / 2.0
      val avgDist = graphDistances.sum / numPairs
      avgDist
    }).seq
    
    lVals.foreach(x => bwL.write(x + "\n"))
    
    bwL.flush
    bwL.close
    
    
  }

  def getCarbonBackbone(pdbFile: File) = {
    val bw = new BufferedReader(new FileReader(pdbFile))
    val carbonCoords = new ArrayBuffer[Point3f]()

    // This sure is ugly...too bad scala.io.Source doesn't work so well
    while (bw.ready) {
      val line = bw.readLine
      if (line.startsWith("ATOM")) {
        val atomType = line.substring(11, 17).trim
        if (atomType == "CA") {
          // ATOM   3602  C   THR   226      20.219   7.787  26.920  0.00  0.00
          val x = line.substring(26, 38).toFloat
          val y = line.substring(38, 46).toFloat
          val z = line.substring(46, 54).toFloat
          val p = new Point3f(x, y, z)
          carbonCoords += p
        }
      }
    }
    bw.close
    carbonCoords.toArray
  }

}