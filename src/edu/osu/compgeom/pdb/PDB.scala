package edu.osu.compgeom.pdb

import java.io._
import javax.vecmath.Point3f
import edu.osu.compgeom.util.IO._

object PDB {
  class AtomName(val element: String, val remoteness: Char, val branchDesignator: Int)

  object AtomName {
    val element = (0 to 1)
    val remoteness = 2
    val branchDesignator = 3
  }

  object AtomRecord {
    val recname = (0 to 5)
    val serial = (6 to 10)
    val atom = (12 to 15)
    val altLoc = (16 to 16)
    val resName = (17 to 19)
    val chainID = (21 to 21)
    val seqNo = (22 to 26)
    val x = (30 to 37)
    val y = (38 to 45)
    val z = (46 to 53)
    val occupancy = (54 to 59)
    val tempFactor = (60 to 65)
    val recID = (72 to 79)
    val segID = (72 to 75)
    val element = (76 to 77)
    val charge = (78 to 79)

    def unapply(s: String): Option[String] = {
      if (s.startsWith("ATOM"))
        Some(s)
      else
        None
    }
  }
  
  def getAlphaCarbonBackbone(pdbFile: File): Array[Point3f] = {
    withBufferedReader(pdbFile) { br =>
      Iterator.continually(br.readLine).takeWhile(_ != null).flatMap{line =>
        if (line.startsWith("ATOM")) {
          // Normally, atom name is 4 characters.  But we don't care about the 
          // last character (branch designator), so we don't extract it here.  Thus
          // the atom variable here is a string of length 3
          val atomName = line.substring(AtomRecord.atom.start, AtomRecord.atom.end)
          assert(atomName.size == 3)
          if (atomName == " CA") {
            val x = line.substring(AtomRecord.x.start, AtomRecord.x.end + 1).toFloat
            val y = line.substring(AtomRecord.y.start, AtomRecord.y.end + 1).toFloat
            val z = line.substring(AtomRecord.z.start, AtomRecord.z.end + 1).toFloat
            Some(new Point3f(x, y, z))
          } else {
            None
          }
        } else {
          None
        }
      }.toArray 
    }   
  }

  def main(args: Array[String]): Unit = {
    import java.io._
    import edu.osu.compgeom.util.IO._
    val inFile = new File("/home/harveywi/research/Protein/survivin/Regular-MD-Explicit/1E31_5_117_NTP_MD.pdb.98")
    val backbone = getAlphaCarbonBackbone(inFile)
    backbone.foreach(println)
  }

}