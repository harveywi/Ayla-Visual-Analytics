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

object GetRadiusOfGyration {
  
  def withBufferedWriter[T](f: File)(op: BufferedWriter => T): T = {
    val bw = new BufferedWriter(new FileWriter(f))
    try {
      op(bw)
    } finally {
      bw.flush
      bw.close
    }
  }

  def main(args: Array[String]): Unit = {
    val dir = new File("/home/harveywi/research/ScalaCrystals/GammaSCrystallin/")

    val dataset = new CachedDataset(dir) with FilePdbStreamProvider
    val lines = scala.io.Source.fromFile(new File(dir, "scalar_functions/potential_energy.txt")).getLines
    val morseFuncVals = lines.drop(1).map(_.toFloat).toArray

    val maxIdx = morseFuncVals.view.zipWithIndex.sortWith((t1, t2) => t1._1 < t2._1).last._2

    val maxPDB = dataset.pdbFiles(maxIdx)

    val cBackbone = getCarbonBackbone(maxPDB)

    // Just use k-means for now.  Initialize clusters randomly
    val assignments = Array.fill[Int](cBackbone.size)(if (math.random > .5) 1 else 0)

    var done = false
    while (!done) {
      val mean1 = new Point3f
      var count1 = 0
      val mean2 = new Point3f
      var count2 = 0
      cBackbone.zip(assignments).foreach {
        case (p, label) => {
          label match {
            case 0 => { mean1.add(p); count1 += 1 }
            case 1 => { mean2.add(p); count2 += 1 }
          }
        }
      }

      mean1.scale(1.0f / count1.toFloat)
      mean2.scale(1.0f / count2.toFloat)

      // Recompute the labels
      val numChanges = cBackbone.zipWithIndex.map {
        case (p, i) => {
          val d1 = mean1.distanceSquared(p)
          val d2 = mean2.distanceSquared(p)
          val oldLabel = assignments(i)
          val newLabel = if (d1 < d2) 0 else 1
          assignments(i) = newLabel
          if (oldLabel != newLabel) 1 else 0
        }
      }.sum

      println("Num changes:  " + numChanges)

      if (numChanges == 0)
        done = true
    }
    
    val n = assignments.size.toFloat
    val zero = new Point3f()
    
    def getRms(subset: Array[Point3f]): Float = {
      val centroid = new Point3f()
      subset.foreach(p => centroid.add(p))
      centroid.scale(1f / n)
      
      val rmsSq = subset.map(p => {
        val q = new Point3f(p)
        q.sub(centroid)
        q.distanceSquared(zero)
      }).sum / n
      math.sqrt(rmsSq).toFloat
    }
    
    // Compute total radius as well as the radius of each monomer independently
    
    val radii = dataset.pdbFiles.par.map(pdbFile => {
      val backbone = getCarbonBackbone(pdbFile)
      val centroidTotal = new Point3f()
      backbone.foreach(p => centroidTotal.add(p))
      centroidTotal.scale(1f / n)
      
      val rmsTotal = getRms(backbone)
      val rmsA = getRms(backbone.indices.filter(assignments(_) == 0).map(backbone(_)).toArray)
      val rmsB = getRms(backbone.indices.filter(assignments(_) == 1).map(backbone(_)).toArray)
      (rmsTotal, rmsA, rmsB)
    }).seq
    
    withBufferedWriter(new File(dir, "scalar_functions/RgTotal.txt")){bw => {
      bw.write("RgTotal\n")
      radii.map(_._1).foreach(x => bw.write(x + "\n"))
    }}
    
    withBufferedWriter(new File(dir, "scalar_functions/RgA.txt")){bw => {
      bw.write("RgA\n")
      radii.map(_._2).foreach(x => bw.write(x + "\n"))
    }}
    
    withBufferedWriter(new File(dir, "scalar_functions/RgB.txt")){bw => {
      bw.write("RgB\n")
      radii.map(_._3).foreach(x => bw.write(x + "\n"))
    }}
    

  }

  def getCarbonBackbone(pdbFile: File) = {
    val br = new BufferedReader(new FileReader(pdbFile))
    val carbonCoords = new ArrayBuffer[Point3f]()

    // This sure is ugly...too bad scala.io.Source doesn't work so well
    while (br.ready) {
      val line = br.readLine
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
    br.close
    carbonCoords.toArray
  }

}