package temp

import edu.osu.compgeom.dataset._
import edu.osu.compgeom.util.IO._
import java.io._
import scopt.OptionParser
import scala.sys.process._
import scala.actors.Futures._
import scala.actors.Actor._
import edu.osu.compgeom.topology.ScalarFunction
import javax.vecmath.Point3f

object TrpCageNeckFormation {

  class CommandLineConfig {
    var datasetDir: File = null
  }

  def main(args: Array[String]): Unit = {
    val config = new CommandLineConfig
    val parser = new OptionParser("DumpSecondaryStructures") {
      arg(
        "<dataset_directory>",
        "Directory where the dataset to analyze is found.", { datasetDir: String =>
          {
            val f = new File(datasetDir)
            require(f.exists, "Error:  The specified dataset directory does not exist.  Exiting.")
            require(f.isDirectory, "Error:  dataset_directory must be a valid directory.  Exiting.")
            config.datasetDir = f
          }
        })
    }

    if (parser.parse(args)) {
      val outDir = new File(config.datasetDir, "scalar_functions/ring/")
      if (!outDir.exists())
        outDir.mkdirs()

      val conformationFilenamesFile = new File(config.datasetDir, "conformation_filenames.txt")

      val dataset = new CachedDataset(config.datasetDir) with FilePdbStreamProvider

      val numConformations = withBufferedReader(conformationFilenamesFile) { br =>
        Iterator.continually(br.readLine()).takeWhile(_ != null).size
      }

      val shortestDistArray = new Array[Float](numConformations)
      val centroidDistArray = new Array[Float](numConformations)
      val hausdorffDistArray = new Array[Float](numConformations)

      val ring1 = (2 to 5)
      val ring2 = (16 to 19)

      def getRing(pdbFile: File): (Array[Point3f], Array[Point3f]) = {
        val atomCoords1 = new scala.collection.mutable.ArrayBuffer[Point3f]
        val atomCoords2 = new scala.collection.mutable.ArrayBuffer[Point3f]
        withBufferedReader(pdbFile) { br =>
          Iterator.continually(br.readLine).takeWhile(_ != null).foreach { line =>
            if (line.startsWith("ATOM")) {
              val seqNo = line.substring(22, 27).trim.toInt
              if (ring1.contains(seqNo)) {
                val x = line.substring(30, 38).toFloat
                val y = line.substring(38, 46).toFloat
                val z = line.substring(46, 54).toFloat
                val p = new Point3f(x, y, z)
                atomCoords1 += p
              } else if (ring2.contains(seqNo)) {
                val x = line.substring(30, 38).toFloat
                val y = line.substring(38, 46).toFloat
                val z = line.substring(46, 54).toFloat
                val p = new Point3f(x, y, z)
                atomCoords2 += p
              }
            }
          }
        }
        (atomCoords1.toArray, atomCoords2.toArray)
      }

      def getShortestDist(set1: Array[Point3f], set2: Array[Point3f]): Float = {
        var curShortestSq = Float.MaxValue
        set1.foreach { p =>
          set2.foreach { q =>
            val dSq = p.distanceSquared(q)
            if (dSq < curShortestSq)
              curShortestSq = dSq
          }
        }
        math.sqrt(curShortestSq).toFloat
      }

      def getCentroidDist(set1: Array[Point3f], set2: Array[Point3f]): Float = {
        val c1 = new Point3f
        set1.foreach(p => c1.add(p))
        c1.scale(1f / set1.size.toFloat)

        val c2 = new Point3f
        set2.foreach(p => c2.add(p))
        c2.scale(1f / set2.size.toFloat)
        c1.distance(c2)
      }

      def getHausdorffDist(set1: Array[Point3f], set2: Array[Point3f]): Float = {
        val shortestDistsSq = Array.fill(set1.size)(Float.MaxValue)
        set1.indices.foreach { i =>
          set2.indices.foreach { j =>
            val dSq = set1(i).distanceSquared(set2(j))
            if (dSq < shortestDistsSq(i))
              shortestDistsSq(i) = dSq
          }
        }
        math.sqrt(shortestDistsSq.max).toFloat
      }

      (0 until numConformations).par.foreach { i =>
        println((i + 1) + " of " + numConformations)
        val (set1, set2) = getRing(dataset.pdbFiles(i))
        shortestDistArray(i) = getShortestDist(set1, set2)
        centroidDistArray(i) = getCentroidDist(set1, set2)
        hausdorffDistArray(i) = getHausdorffDist(set1, set2)
      }

      withBufferedWriter(new File(outDir, "ringShortestDist.txt")) { bw =>
        bw.write("Ring Shortest Dist\n")
        shortestDistArray.foreach(f => bw.write(f + "\n"))
      }

      withBufferedWriter(new File(outDir, "ringCentroidDist.txt")) { bw =>
        bw.write("Ring Centroid Dist\n")
        centroidDistArray.foreach(f => bw.write(f + "\n"))
      }

      withBufferedWriter(new File(outDir, "ringHausdorffDist.txt")) { bw =>
        bw.write("Ring Hausdorff Dist\n")
        hausdorffDistArray.foreach(f => bw.write(f + "\n"))
      }

      //      // Find out how many residues there are
      //      val numResidues = {
      //        withBufferedReader(conformationFilenamesFile) { br =>
      //          // Get last line of some PDB file in the dataset, and grab the residue ID of it.
      //          // That will equal the number of residues in the protein
      //          val somePdbFile = new File(Iterator.continually(br.readLine).next())
      //          val lastPdbLine = withBufferedReader(somePdbFile) { br => Stream.continually(br.readLine).takeWhile(_ != null).filter(_.startsWith("ATOM")).last }
      //          val lastResidueID = lastPdbLine.substring(22, 27).trim.toInt
      //          lastResidueID
      //        }
      //      }
      //      println("Num residues:  " + numResidues)
      //
      //      val dsspFile = new File(config.datasetDir, "secondary_structure_dssp.dat")
      //      val ois = new ObjectInputStream(new FileInputStream(dsspFile))
      //      val structureInfo = ois.readObject.asInstanceOf[Array[Char]]
      //      println(structureInfo.size)
      //      println(numConformations * numResidues)
      //
      //      val ssMap = Map('X' -> "structure=0",
      //        'H' -> "helixalpha",
      //        'B' -> "sheet",
      //        'E' -> "sheet",
      //        'G' -> "helix310",
      //        'I' -> "helixpi",
      //        'T' -> "turn",
      //        'S' -> "bend" /* 'S' -> "bend" */ )
      //
      //      def getSSInfo(conformationID: Int) = {
      //        val ofs = numResidues * conformationID
      //        Iterator.range(0, numResidues).map(i => structureInfo(ofs + i))
      //      }
      //
      //      val dataset = new CachedDataset(config.datasetDir)
      //      val lines = scala.io.Source.fromFile(new File(config.datasetDir, "scalar_functions/potential_energy.txt")).getLines
      //      val morseFuncVals = lines.drop(1).map(_.toFloat).toArray
      //
      //      val vertices = dataset.pcaPoints.toArray
      //
      //      println("Constructing Morse function")
      //      val morseFunction = new ScalarFunction(vertices, Array.empty[Array[Int]], morseFuncVals)
      //
      //      val idxNative = morseFuncVals.indices.sortWith((i, j) => morseFunction.vc.compare(i, j) < 0).head
      //      val ssNative = getSSInfo(idxNative).toArray
      //
      //      for ((letter, description) <- ssMap if letter != 'X') {
      //
      //        withBufferedWriter(new File(outDir, description + ".txt")) { bw =>
      //          bw.write(description + "\n")
      //          (0 until numConformations).foreach { id =>
      //            val count = ssNative.zip(getSSInfo(id).toSeq).count { case (x, y) => { x != letter && y == letter } }
      //            bw.write(count + "\n")
      //          }
      //        }
      //      }
    } else {
      parser.showUsage
      System.exit(0)
    }

  }
}