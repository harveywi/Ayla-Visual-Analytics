package edu.osu.compgeom.dataset

import java.io._
import edu.osu.compgeom.ayla.AylaClient
import edu.osu.compgeom.util.IO._
import org.apache.commons.compress.archivers.zip._
import javax.vecmath.Point3f
import scala.util.matching.Regex

trait PdbStreamProvider {
  def getPDBLines(i: Int): Array[String]
  def getPDBInputStream(i: Int): InputStream
  val size: Int

  def getCarbonBackbone(i: Int): Array[Point3f] = {
    withBufferedReader(getPDBInputStream(i)) { br =>
      val carbonCoords = new scala.collection.mutable.ArrayBuffer[Point3f]()
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
      carbonCoords.toArray
    }
  }

  def getBackboneVectorK5(idx: Int): Array[Double] = {
    val carbonBackbone = getCarbonBackbone(idx)
    val n = carbonBackbone.length
    val ret = new Array[Double](4 * n - 10)

    var i = 0
    var retIdx = 0
    while (i < n) {
      var ofs = 1
      var j = i + ofs
      while (ofs < 5 && j < n) {
        ret(retIdx) = carbonBackbone(i).distance(carbonBackbone(j))
        retIdx += 1
        j += 1
        ofs += 1
      }

      i += 1
    }

    return ret
  }

  def getBackbonePairwiseDistVec(i: Int): Array[Double] = {
    val carbonBackbone = getCarbonBackbone(i)
    // Compute pairwise distances between all the carbon atoms
    val pairwiseDists = (0 until carbonBackbone.size).par.flatMap(i => {
      (i + 1 until carbonBackbone.size).map(j => {
        carbonBackbone(i).distance(carbonBackbone(j)).toDouble
      })
    })
    pairwiseDists.toArray
  }

  def getSurvivinF93_F101_PairwiseDistVec(i: Int): Array[Double] = {
    val carbonBackbone = getCarbonBackbone(i)
    val roi = carbonBackbone.slice(88, 97)
    val pairwiseDists = (0 until roi.size).par.flatMap(i => {
      (i + 1 until roi.size).map(j => {
        roi(i).distance(roi(j)).toDouble
      })
    })
    pairwiseDists.toArray
  }
  
  // Number of residue pairs whose distance is <= 7 angstroms
  def getContactDensity(conformationID: Int, residues: Array[Int]): Int = {
    val backbone = getCarbonBackbone(conformationID)
    val densityThresh = 7*7
    var idx1 = 0
    var contactDensity = 0
    while (idx1 < backbone.length) {
      val i = residues(idx1)
      var idx2 = idx1 + 1
      while (idx2 < backbone.length) {
        val j = residues(idx2)
        val dist = backbone(i).distanceSquared(backbone(j))
        if (dist <= densityThresh) {
          contactDensity += 1
        }
        idx2 += 1
      }
      idx1 += 1
    }
    contactDensity
  }
}

trait AylaClientPdbStreamProvider extends PdbStreamProvider {
  val client: AylaClient
  def getPDBLines(i: Int) = client.getPDBLines(i)

  def getPDBInputStream(i: Int): InputStream = {
    val pdbLines = getPDBLines(i)
    new ByteArrayInputStream(pdbLines.mkString("\n").getBytes())
  }
}

trait ZipFilePdbStreamProvider extends PdbStreamProvider {
  val dir: File
  final val zipPaths: Array[String] = withBufferedReader(new File(dir, "conformation_zip_paths.txt")) { br =>
    Iterator.continually(br.readLine).takeWhile(_ != null).toArray
  }
  
  val size = zipPaths.size

  private[this] val zipFile = new ZipFile(new File(dir, "conformations.zip"))

  def getPDBLines(i: Int): Array[String] = {
    val zipInputStream = getPDBInputStream(i)
    val br = new BufferedReader(new InputStreamReader(zipInputStream))
    val lines = Stream.continually(br.readLine).takeWhile(_ != null).toArray
    br.close
    lines
  }

  def getPDBInputStream(i: Int): InputStream = {
    val zipPath = zipPaths(i)
    val entry = zipFile.getEntry(zipPath)
    val zipInputStream = zipFile.getInputStream(entry)
    zipInputStream
  }

  override def finalize(): Unit = {
    zipFile.close()
  }

  def findMatchingConformations(regex: Regex): Array[(String, Int)] = {
    zipPaths.zipWithIndex.filter{case (path, idx) => regex.pattern.matcher(path).matches}.toArray
  }
}

trait FilePdbStreamProvider extends PdbStreamProvider {
  val dir: File
  val pdbFiles: Array[File] = withBufferedReader(new File(dir, "conformation_filenames.txt")) { br =>
    Iterator.continually(br.readLine).takeWhile(_ != null).map(new File(_)).toArray
  }
  val size = pdbFiles.size

  def getPDBLines(i: Int): Array[String] = {
    val zipInputStream = getPDBInputStream(i)
    withBufferedReader(getPDBInputStream(i)) { br =>
      Stream.continually(br.readLine).takeWhile(_ != null).toArray
    }
  }

  def getPDBInputStream(i: Int): InputStream = {
    return new FileInputStream(pdbFiles(i))
  }

  def findMatchingConformations(regex: Regex): Array[(String, Int)] = {
    pdbFiles.iterator.zipWithIndex.flatMap {
      case (pdbFile, idx) =>
        val absPath = pdbFile.getAbsolutePath()
        if (regex.pattern.matcher(absPath).matches) {
          Some((absPath, idx))
        } else {
          None
        }
    }.toArray
  }
}
