/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.preprocess

import java.io._
import java.nio.channels.FileChannel
import java.nio.ByteBuffer
import ayla.pdb._
import ayla.util.IO._
import ayla.linalg._
import ayla.util.Timing
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.Executors
import scalaz.Validation
import scalaz.Success
import scalaz.Failure
import scalaz.Validation._

object PointCloudMaker {
  def main(args: Array[String]): Unit = parseArgs(args).fold(errorMsg => println(errorMsg), Function.tupled(makePointCloud _)(_))

  def parseArgs(args: Array[String]): Validation[String, (File, PdbStreamProvider)] = {
    val usage = "Usage:  pointCloudMaker dataset_dir"

    def catchException[T](op: => T) = fromTryCatch(op).fail.map(e => s"An error occurred:  ${e.getMessage}").validation
    
    def parseDatasetDir(arg: String) = new File(arg) match {
      case f if f.exists && f.isDirectory => Success(f)
      case f @ _ => Failure(s"The dataset directory '${f.getAbsolutePath}' is not valid.")
    }

    args match {
      case Array(datasetDirArg) =>
        for {
          datasetDir <- parseDatasetDir(datasetDirArg)
          pdbStreamProvider <- PdbStreamProvider(datasetDir)
        } yield (datasetDir, pdbStreamProvider)
      case _ => Failure(usage)
    }
  }

  def makePointCloud(datasetDir: File, pdbProvider: PdbStreamProvider): Unit = {
    val pcaSampleSize = math.min(pdbProvider.size, 3000)
    val sampleIndices = Range(0, pdbProvider.size).map((_, math.random)).sorted.map(_._1).take(pcaSampleSize)

    val pcaSample = sampleIndices.map(pdbProvider.getBackbonePairwiseDistVec).toArray
    val pcaSampleMean = getMeanVec(pcaSample)

    val evecs = Timing("Performing (approximate) PCA") {
      pcaSample.foreach { s =>
        s.indices.foreach(i => s(i) -= pcaSampleMean(i))
      }
      PCA.getPrincipalComponents(pcaSample, math.min(pcaSample.length - 1, 500), pcaSampleMean)
    }

    val pcd = new Array[Array[Float]](pdbProvider.size)

    val progress = new AtomicInteger()
    Timing("Projecting conformations onto approximate PCA basis") {
      (0 until pdbProvider.size).par.foreach { conformationID =>
        val p = pdbProvider.getBackbonePairwiseDistVec(conformationID)
        pcd(conformationID) = evecs.map(evec => pcaProject(p, evec, pcaSampleMean)).toArray
        val prog = progress.incrementAndGet
        if (prog % 1000 == 0) {
          println("Progress:  %.2f".format(100 * prog / pcd.length.toDouble) + "%")
        }
      }
    }

    PointCloudData.save(pcd, new File(datasetDir, "pcd_v3.dat"))
    println("Done.")
  }

  def pcaProject(p: Array[Double], evec: Array[Double], pcaSampleMean: Array[Double]): Float = {
    var i = 0
    var curSum = 0d
    while (i < p.length) {
      curSum += (p(i) - pcaSampleMean(i)) * evec(i)
      i += 1
    }
    curSum.toFloat
  }

  def getMeanVec(pts: Array[Array[Double]]): Array[Double] = {
    val pMean = new Array[Double](pts(0).length)
    pts.foreach { p =>
      p.indices.foreach(j => pMean(j) += p(j))
    }
    pMean.indices.foreach(j => pMean(j) /= pts.length.toDouble)

    return pMean
  }

}
