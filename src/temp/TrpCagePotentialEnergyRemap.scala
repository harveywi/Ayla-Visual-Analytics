package temp

import edu.osu.compgeom.util.IO._
import java.io._

object TrpCagePotentialEnergyRemap {

  def main(args: Array[String]): Unit = {
    val dir = new File("/home/harveywi/research/ScalaCrystals/TrpCage")
    val oldEnergies = withBufferedReader(new File(dir, "scalar_functions/potential_energy.txt")) {br =>
      // skip first line
      br.readLine
      Iterator.continually(br.readLine).takeWhile(_ != null).map(_.toFloat).toArray
    }
    
    val c1 = "/media/intel_ssd/TrpCage/1079_2401.pdb"
    val c2 = "/media/intel_ssd/TrpCage/1130_2067.pdb"
      
    val conformationFilenames = withBufferedReader(new File(dir, "conformation_filenames.txt")){br =>
      Iterator.continually(br.readLine).takeWhile(_ != null).toArray
    }
    
    assert(conformationFilenames.size == oldEnergies.size + 2)
    
    import scala.collection.mutable.Queue
    val s1 = new Queue[Float]
    s1.enqueue(oldEnergies: _*)
    val s2 = new Queue[String]
    s2.enqueue(conformationFilenames: _*)
    
    val newEnergies = new scala.collection.mutable.ArrayBuffer[Float]
    while (!s2.isEmpty) {
      val name = s2.dequeue
      if (name == c1 || name == c2) {
        newEnergies += newEnergies.last
      } else {
        newEnergies += s1.dequeue
      }
    }
    
    assert(newEnergies.size == conformationFilenames.size)
    
    withBufferedWriter(new File(dir, "potential_energies2.txt")){bw =>
      bw.write("Potential Energy\n")
      newEnergies.foreach(e => bw.write(e + "\n"))
    }
    
  }

}