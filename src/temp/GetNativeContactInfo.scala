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
import edu.osu.compgeom.dataset.FilePdbStreamProvider

object GetNativeContactInfo {

  class NativeContact(val i: Int, val j: Int, val dist: Double)

  def main(args: Array[String]): Unit = {
    val dir = new File("/home/harveywi/research/ScalaCrystals/TrpCage/")
    
    val bwQ = new BufferedWriter(new FileWriter(new File(dir, "scalar_functions/q_nmr.txt")))
    bwQ.write("Q\n")
    
    val bwQs = new BufferedWriter(new FileWriter(new File(dir, "scalar_functions/qs_nmr.txt")))
    bwQs.write("Qs\n")

    val dataset = new CachedDataset(dir) with FilePdbStreamProvider
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
    
    // Now calculate Q and Qs.
    val scaleFactor = 1 / ((numNativeContacts - 1) * (numNativeContacts - 2))
    val qPairs = dataset.pdbFiles.par.map(pdbFile => {
      val backbone = getCarbonBackbone(pdbFile)
      
      val valuesToSum = nativeContacts.flatMap(nc => {
        val dist = backbone(nc.i).distance(backbone(nc.j))
        if (dist <= densityThresh) {
          val rDiffSq = (nc.dist - dist) * (nc.dist - dist)
          val sigma = math.pow(math.abs(nc.i - nc.j), 0.15)
          Some(math.exp(-1 * rDiffSq / (sigma*sigma)))
        } else {
          None
        }
      })
      
      val q = valuesToSum.size
      val qs = scaleFactor * valuesToSum.sum
      (q, qs)
    }).seq
    
    qPairs.foreach{case (q, qs) => {
      bwQ.write(q + "\n")
      bwQs.write(qs + "\n")
    }}
    bwQ.flush
    bwQ.close
    bwQs.flush
    bwQs.close
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