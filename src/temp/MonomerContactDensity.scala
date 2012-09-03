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
import edu.osu.compgeom.dataset._

object MonomerContactDensity {

  def main(args: Array[String]): Unit = {
    val dir = new File("/home/harveywi/research/ScalaCrystals/Survivin_252996/")

    val dataset = new CachedDataset(dir) with ZipFilePdbStreamProvider
    val lines = scala.io.Source.fromFile(new File(dir, "scalar_functions/potential_energy.txt")).getLines
    val morseFuncVals = lines.drop(1).map(_.toFloat).toArray    
    
    val maxIdx = morseFuncVals.view.zipWithIndex.sortWith((t1, t2) => t1._1 < t2._1).last._2

    val cBackbone = dataset.getCarbonBackbone(maxIdx)
    
    // Just use k-means for now.  Initialize clusters randomly
    val assignments = Array.fill[Int](cBackbone.size)(if (math.random > .5) 1 else 0)
    
    var done = false
    while (!done) {
      val mean1 = new Point3f
      var count1 = 0
      val mean2 = new Point3f
      var count2 = 0
      cBackbone.zip(assignments).foreach{case (p, label) => {
        label match {
          case 0 => {mean1.add(p); count1 += 1}
          case 1 => {mean2.add(p); count2 += 1}
        }
      }}
      
      mean1.scale(1.0f / count1.toFloat)
      mean2.scale(1.0f / count2.toFloat)
      
      // Recompute the labels
      val numChanges = cBackbone.zipWithIndex.map{case (p, i) => {
        val d1 = mean1.distanceSquared(p)
        val d2 = mean2.distanceSquared(p)
        val oldLabel = assignments(i)
        val newLabel = if (d1 < d2) 0 else 1
        assignments(i) = newLabel
        if (oldLabel != newLabel) 1 else 0
      }}.sum
      
      println("Num changes:  " + numChanges)
      
      if (numChanges == 0)
        done = true
    }
    
    if (assignments(0) == 1) {
      assignments.indices.foreach{i => if (assignments(i) == 0) assignments(i) = 1 else assignments(i) = 0}
    }
    
    // Now we can go through each conformation and compute its per-monomer contact density.
    val densityThresh = 7;
    val n = assignments.size
    
    val allContacts = Array.fill(dataset.pcaPoints.length)(new Array[Array[Int]](cBackbone.length))
    
    val densityTriples = dataset.pcaPoints.indices.par.map(idx => {
      val backbone = dataset.getCarbonBackbone(idx)
      var totalDensity = 0
      var density1 = 0
      var density2 = 0
      
      val contactPairBuffs = Array.fill(cBackbone.length)(new ArrayBuffer[Int])//new ArrayBuffer[Array[Int]]
      
      var i = 0
      while (i < n) {
        var j = i+1
        while (j < n) {
          val dist = backbone(i).distance(backbone(j))
          if (dist <= densityThresh) {
	          (assignments(i), assignments(j)) match {
	            case (0, 0) => {density1 += 1}
	            case (1, 1) => {density2 += 1}
	            case _ => {}
	          }
	          contactPairBuffs(i) += j
	          totalDensity += 1
          }
          j += 1
        }
        i += 1
      }
      
      allContacts(idx) = contactPairBuffs.map(_.toArray)
      
//      contactPairs.groupBy(_(0))
      
      (density1, density2, totalDensity)
    }).seq
    
    val bwMonomer1 = new BufferedWriter(new FileWriter(new File("/home/harveywi/Desktop/cd_monomer_1.txt")))
    val bwMonomer2 = new BufferedWriter(new FileWriter(new File("/home/harveywi/Desktop/cd_monomer_2.txt")))
    val bwContactDensity = new BufferedWriter(new FileWriter(new File("/home/harveywi/Desktop/contact_density.txt")))
    
    import edu.osu.compgeom.util.IO._
    withBufferedWriter(new File("/home/harveywi/Desktop/cd_monomer_diff.txt")){bw =>
      bw.write("CD Mon2 - Mon1\n")
      densityTriples.foreach{t =>
        bw.write((t._2 - t._1) + "\n")
      }
    }
    
    withObjectOutputStream(new File("/home/harveywi/Desktop/contactPairs.dat")){oos =>
      oos.writeObject(allContacts)
//      allContacts.zipWithIndex.foreach{case (contacts, i) =>
//        bw.write(i + "\n")
//        t._4.foreach(pair => bw.write("\t" + pair(0) + "," + pair(1) + "\n"))
//      }
    }
    
    bwMonomer1.write("CD Monomer 1\n")
    bwMonomer2.write("CD Monomer 2\n")
    bwContactDensity.write("Contact Density\n")
    
    densityTriples.foreach(t => {
      bwMonomer1.write(t._1 + "\n")
      bwMonomer2.write(t._2 + "\n")
      bwContactDensity.write(t._3 + "\n")
    })

   List(bwMonomer1, bwMonomer2, bwContactDensity).foreach(bw => {bw.flush; bw.close})
  }
}