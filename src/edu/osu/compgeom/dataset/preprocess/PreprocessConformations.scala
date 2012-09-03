package edu.osu.compgeom.dataset.preprocess

import java.io._
import scala.collection.mutable.ArrayBuffer
import javax.vecmath.Point3f
import edu.osu.compgeom.linalg.PCA
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scopt.OptionParser

object PreprocessConformations {

  class CommandLineConfig {
    var datasetDir: File = null
    var pcaSampleSize: Int = 100
  }

  def main(args: Array[String]): Unit = {
    val config = new CommandLineConfig
    val parser = new OptionParser("PreprocessConformations") {
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

      arg(
        "<pca_sample_size>",
        "Number of sample points to use for approximating the PCA basis of the whole point cloud (must be >= 3).", { s: String =>
          {
            config.pcaSampleSize = s.toInt
            require(config.pcaSampleSize >= 3, "pca_sample_size must be >= 3.")
          }
        })
    }
    
    if (parser.parse(args)) {
      val conformationFilenamesFile = new File(args(0), "conformation_filenames.txt")
      require(conformationFilenamesFile.exists, "Error:  The file called 'conformation_filenames.txt' was not found in %s.".format(config.datasetDir.getAbsolutePath()))
    
      def withBufferedReader[T](f: File)(op: BufferedReader => T): T = {
        val br = new BufferedReader(new FileReader(f))
        try {
          op(br)
        } finally {
          br.close
        }
      }
      
      val numConformations = withBufferedReader(conformationFilenamesFile)(br => {
        Iterator.continually(br.readLine()).takeWhile(_ != null).size
      })
      
      println("Number of conformations:  " + numConformations)
      
      val sampleIndices = Range(0, numConformations).map((_, math.random)).sortWith((t1, t2) => t1._2 < t2._2).take(config.pcaSampleSize).map(_._1).toSet
      
      println("Sampling point cloud data for PCA")
      val pcaSample = withBufferedReader(conformationFilenamesFile)(br => {
        Iterator.continually(br.readLine()).takeWhile(_ != null).zipWithIndex.flatMap {
		      case (pdbFilename, i) => if (sampleIndices.contains(i))
		        Some(conformationFileToBackboneDistVec(new File(pdbFilename)).map(_.toDouble))
		      else
		        None
		    }.toArray
      })
      
      println("Performing (approximate) PCA")
      val pcaSampleMean = getMeanVec(pcaSample)
      val evecs = PCA.getPrincipalComponents(pcaSample, 100, pcaSampleMean)
      
      // Output files
      val pcdOutFile = new File(config.datasetDir, "pcd.dat")
      val pca3dOutFile = new File(config.datasetDir, "pca3D.dat")
      List(pcdOutFile, pca3dOutFile).foreach(f => if (f.exists) f.delete())
      
	    val pcdOut = new RandomAccessFile(pcdOutFile, "rw")
	    pcdOut.writeInt(numConformations)
	    pcdOut.writeInt(evecs.size)
	    
	    val pca3dOut = new RandomAccessFile(pca3dOutFile, "rw")
      pca3dOut.writeInt(numConformations)
      

      // This method subtracts the mean vector from p, and computes a dot product with evec.
      // Written recursively with tail call optimization to get performance comparable to a while loop.
      @tailrec
	    def pcaProject(p: Array[Double], evec: Array[Double], i: Int = 0, curSum: Double = 0): Float = {
	      if (i == p.size)
	        curSum.toFloat
	      else {
	        val curSumNew = curSum + (p(i) - pcaSampleMean(i)) * evec(i)
	        pcaProject(p, evec, i + 1, curSumNew)
	      }
	    }
      
      println("Projecting conformations onto approximate PCA basis")
      withBufferedReader(conformationFilenamesFile)(br => {
		    Iterator.continually(br.readLine()).takeWhile(_ != null).foreach(line => {
		      val pdbFile = new File(line)
		      val p = conformationFileToBackboneDistVec(pdbFile)
		      val pcaProj = evecs.par.map(evec => pcaProject(p, evec)).seq
		      pcaProj.foreach(pcdOut.writeFloat(_))
		      pcaProj.take(3).foreach(pca3dOut.writeFloat(_))
		    })        
      })
      
      pcdOut.close
	    pca3dOut.close
	
	    println("Calculating pairwise distances")
	    calcPairwiseDistances(config.datasetDir, 8)
      
      println("Done!")
    } else {
      System.exit(0)
    }
  }

  def getMeanVec(X: Array[Array[Double]]): Array[Double] = {
    val numRows = X.size
    val numColumns = X(0).size
    val mean = new Array[Double](numColumns)
    (0 until numRows).foreach(row => {
      (0 until numColumns).foreach(column => {
        mean(column) += X(row)(column)
      })
    })

    (0 until numColumns).foreach(column => {
      mean(column) /= numRows.toDouble
    })

    return mean
  }

  def conformationFileToBackboneDistVec(pdbFile: File): Array[Double] = {
    val br = new BufferedReader(new FileReader(pdbFile))
    val atomCoords = new ArrayBuffer[Point3f]()

    while (br.ready) {
      val line = br.readLine
      if (line.startsWith("ATOM")) {
        val atomType = line.substring(12, 16).trim
        if (atomType == "CA") {
          val x = line.substring(30, 38).toFloat
			    val y = line.substring(38, 46).toFloat
			    val z = line.substring(46, 54).toFloat
          val p = new Point3f(x, y, z)
          atomCoords += p
        }
      }
    }
    br.close
    
    // Compute pairwise distances between all the carbon atoms
    val pairwiseDists = (0 until atomCoords.size).par.flatMap(i => {
      (i + 1 until atomCoords.size).map(j => {
        atomCoords(i).distance(atomCoords(j)).toDouble
      })
    })
    pairwiseDists.toArray
  }

  def calcPairwiseDistances(outDir: File, numThreads: Int): Unit = {
    val pcdFile = new RandomAccessFile(new File(outDir, "pcd.dat"), "r")

    val numPts = pcdFile.readInt
    val nDims = pcdFile.readInt
    
    val pairsFile = new File(outDir, "distPairs.dat")
    if (pairsFile.exists)
      pairsFile.delete()

    val pairsOutFile = new RandomAccessFile(new File(outDir, "distPairs.dat"), "rw")
    pairsOutFile.setLength(4 + numPts * (math.min(100, numPts - 1) * 8))
    pairsOutFile.writeInt(numPts)

    @tailrec
    def euclidDist(p1: Array[Float], p2: Array[Float], i: Int = 0, sumSq: Double = 0): Float = {
      if (i == nDims)
        math.sqrt(sumSq).toFloat
      else {
        val diff = p1(i) - p2(i)
        euclidDist(p1, p2, i+1, sumSq + diff*diff)
      }
    }
    
    def loadSomePoint(i: Int): Array[Float] = {
      pcdFile.seek(8 + 4 * i * nDims)
      return (for (i <- 1 to nDims) yield pcdFile.readFloat).toArray
    }

    val pts = (0 until numPts).map(loadSomePoint).toArray
    
    class DistPair(val id: Int, val dist: Float) extends Comparable[DistPair] {
      override def compareTo(o: DistPair) = java.lang.Float.compare(o.dist, dist)
    }

    // Compute sorted distance/index pairs for each point and save them in a file
    import scala.collection.JavaConversions._
    val lock2 = new scala.concurrent.Lock
    val executorService = Executors.newFixedThreadPool(numThreads)
    (0 until numPts).foreach { i =>
      val runnable = new Runnable() {
        override def run: Unit = {
          println((i + 1) + " of " + numPts)
          val p1 = pts(i)

          /*
          val distPairs =
            (0 until numPts).view.map { j =>
              val p2 = pts(j)
              val dist = euclidDist(p1, p2)
              (j, dist)
            }.sortWith((t1, t2) => t1._2 < t2._2).drop(1).take(100).force
          */
          var dist101 = Float.MaxValue
          val pq = new java.util.PriorityQueue[DistPair]
          (0 until numPts).foreach{j =>
            val p2 = pts(j)
            val dist = euclidDist(p1, p2)
            if (dist < dist101) {
              pq.add(new DistPair(j, dist))
              if (pq.size >= 101)
                dist101 = pq.peek().dist
              if (pq.size > 101)
                pq.poll()
            }
          }
          val distPairs = pq.toIndexedSeq.sortWith((p1, p2) => p1.dist < p2.dist).drop(1)

          lock2.acquire
          pairsOutFile.seek(4 + 8 * (math.min(numPts - 1, 100)) * i)
          distPairs.foreach { pair =>
            pairsOutFile.writeInt(pair.id)
            pairsOutFile.writeFloat(pair.dist)
          }
          lock2.release
        }
      }
      executorService.execute(runnable)
    }
    executorService.shutdown
    executorService.awaitTermination(100000, TimeUnit.DAYS)

    pcdFile.close
    pairsOutFile.close
  }
}