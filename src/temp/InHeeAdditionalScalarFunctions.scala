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

object InHeeAdditionalScalarFunctions {

  def main(args: Array[String]): Unit = {
    val dir = new File("/home/harveywi/research/ScalaCrystals/Survivin_252996/")
    val dataset = new CachedDataset(dir) with FilePdbStreamProvider
    
    loadInHeeData(dataset.pdbFiles)
    
  }
  
  def loadInHeeData(pdbFiles: Array[File]): Unit = {
    val dir = new File("/home/harveywi/research/in-hee_2011_11_14_0630")
    
    val summaryRegularMD = new File(dir, "Survivin_NTP_300K_rec.all.out.SUMMARY")
    val summariesREMD = new File(dir, "REMD_MMGBSA").listFiles()
    val summariesREMD_new = new File(dir, "MMGBSA_largeScale").listFiles()
    
    // Need to map each survivin directory to its summary file.
    val dirMap = new scala.collection.mutable.HashMap[File, File]
    dirMap(new File("/home/harveywi/research/Protein/survivin/Regular-MD-Explicit")) = summaryRegularMD
    
    val remdDirs = new File("/home/harveywi/research/Protein/survivin/REMD_explicit_water").listFiles.filter(_.isDirectory)
    remdDirs.foreach(remdDir => {
      val temperature = remdDir.getName().takeRight(6)
      dirMap(remdDir) = summariesREMD.find(_.getName.contains(temperature)).get
    })
    
    val remdNewDirs = new File("/home/harveywi/research/Protein/survivin/REMD_new").listFiles.filter(_.isDirectory)
    remdNewDirs.foreach(remdNewDir => {
      val temperature = remdNewDir.getName().takeRight(6)
      dirMap(remdNewDir) = summariesREMD_new.find(_.getName.contains(temperature)).get
    })
    
    val funcNames = Array("E_bond", "E_angle", "E_dihed", "E_els", "E_vdw", "E_gbsa", "TOT_PE", "TOT_PE_noHighF")
    
    val bws = funcNames.map(name => new BufferedWriter(new FileWriter(new File(dir, name + ".txt"))))
    bws.zip(funcNames).foreach{case (bw, funcName) => {
      bw.write(funcName + "\n")
    }}
    
    val summaryFileToParsedVectors = dirMap.values.map(summaryFile => {
      (summaryFile, parseSummaryFile(summaryFile))
    }).toMap
    
    val defaultVec = summaryFileToParsedVectors(dirMap(pdbFiles(0).getParentFile))(1)
    
    pdbFiles.foreach(pdbFile => {
      val summaryFile = dirMap(pdbFile.getParentFile())
      val lastIdxPeriod = pdbFile.getName.lastIndexOf('.')
      val idx = pdbFile.getName.substring(lastIdxPeriod + 1).toInt
      val vecOption = summaryFileToParsedVectors(summaryFile).get(idx)
      vecOption match {
        case Some(vec) => {
		      vec.zipWithIndex.foreach{case (f, i) => {
		        bws(i).write(f + "\n")
		      }}         
        }
        case None => {
          println("Warning:  Conformation " + pdbFile + " doesn't have values.")
          defaultVec.zipWithIndex.foreach{case (f, i) => {
		        bws(i).write(f + "\n")
		      }}
        }
      }

    })
    
    bws.foreach(bw => {
      bw.flush()
      bw.close()
    })
    
  }
  
  def parseSummaryFile(f: File): Map[Int, Array[Float]] = {
    val br = new BufferedReader(new FileReader(f))
/*
Survivin_NTP_300K     1       660.94    1880.61       2416.70     -2743.59    -911.20    -4865.21       -3561.75    -6103.29
 */
    val pat = "\\s+".r
    
    val ret = new scala.collection.mutable.HashMap[Int, Array[Float]]
    br.readLine	// skip first line
    while (br.ready) {
      val line = br.readLine
      val lineSplit = pat.split(line)
      assert(lineSplit.size == 10)
      val idx = lineSplit(1).toInt
      val vec = lineSplit.drop(2).map(_.toFloat).toArray
      ret += (idx -> vec)
    }
    
    br.close
    ret.toMap
  }

}