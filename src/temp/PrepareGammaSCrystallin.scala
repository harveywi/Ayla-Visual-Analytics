package temp

import java.io._
import edu.osu.compgeom.util.IO._

object PrepareGammaSCrystallin {

  def main(args: Array[String]): Unit = {
    dumpEnergies
    println("Done")
  }
  
  def dumpEnergies = {
    val dir = new File("/home/harveywi/research/summary_gammas")
    val files = dir.listFiles.filter(_.getName.endsWith("FINAL")).sortWith((f1, f2) => f1.getName < f2.getName)
    val pat = "\\s+".r
    
    withBufferedWriter(new File("/home/harveywi/research/ScalaCrystals/GammaSCrystallin/cfNew.txt")){bwCF =>
      withBufferedWriter(new File("/home/harveywi/research/ScalaCrystals/GammaSCrystallin/scalar_functions/potential_energy.txt")){bwEnergy =>
      	files.foreach{f =>
      		withBufferedReader(f){br =>
      		  br.readLine
      		  while (br.ready) {
      		    val line = br.readLine
      		    val lineSplit = pat.split(line)
      		    if (lineSplit.size >= 9) {
      		      val cf = "/media/intel_ssd/gammaS_Crystallin_ReservoirREMD/" + lineSplit(0)  
      		      val energy = lineSplit(9).toFloat
      		      bwCF.write(cf + "\n")
      		      bwEnergy.write(energy + "\n")
      		    }
      		  }
      		}
      	}
      }
    }
    println("Done")
    
//    var total = 0
//    var sumErr = 0
//    files.foreach{f =>
//      val energies = withBufferedReader(f){br =>
//        br.readLine
//        Iterator.continually(br.readLine).takeWhile(_ != null).flatMap{line =>
//          val ps = pat.split(line)
//          if (ps.size < 9) {
//            sumErr += 1
//            println(line)
//            None
//          } else {
//            Some(pat.split(line)(9).toFloat)
//          }
//        }.toArray
//      }
//      total += energies.size
//    }
//    println("Total energies:  " + total)
//    println("Total err:  " + sumErr)
//    println("Overall: " + (total + sumErr))
  }
  
  
  def dumpConformationFilenames = {
    val dir = new File("/media/intel_ssd/gammaS_Crystallin_ReservoirREMD")
    val pdbFilesSorted = dir.listFiles.filter(_.getName.startsWith("rremd")).sortWith{(f1, f2) =>
      val prefix1 = f1.getName.takeWhile(_ != 'K')
      val prefix2 = f2.getName.takeWhile(_ != 'K')
      if (prefix1 == prefix2) {
	      val id1 = f1.getName.substring(f1.getName.lastIndexOf('.') + 1).toInt
	      val id2 = f2.getName.substring(f2.getName.lastIndexOf('.') + 1).toInt
      	id1 < id2
      } else {
        prefix1 < prefix2
      }
    }
    
    println("Number of conformations:  " + pdbFilesSorted.size)
    
    withBufferedWriter(new File("/home/harveywi/research/ScalaCrystals/GammaSCrystallin/conformation_filenames.txt")) {bw =>
    	pdbFilesSorted.foreach(file => bw.write(file.getAbsolutePath + "\n"))
    }
  }

}