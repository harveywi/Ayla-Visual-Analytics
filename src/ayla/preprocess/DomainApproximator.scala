/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.preprocess

import ayla.util.IO._
import java.io._
import scala.io.Source
import ayla.pdb._
import ayla.dataset.CachedDataset
import scalaz.{ Validation, Success, Failure }
import scalaz.Validation._
import org.apache.commons.compress.archivers.zip.ZipFile
import ayla.util.Tools._
import ayla.util.Timing
import ayla.geometry.NearestNeighborSearcher
import ayla.geometry.SimplicialComplex
import java.util.concurrent.atomic.AtomicInteger

object DomainApproximator {

  type Filter = String => Boolean

  def main(args: Array[String]): Unit = parseArgs(args).fold(
    errorMsg => println(errorMsg),
    Function.tupled(buildDomain _)(_))

  def parseArgs(args: Array[String]): Validation[String, (Filter, Int, File, File)] = {
    val usage = """|Usage:  domainApproximator [-w whitelist | -b blacklist] k dataset_dir outputFunctionName
          				 |The -w option will keep the conformations in the given whitelist text file and ignore all others.
    							 |The -b option will discard the conformations in the given blacklist text file and ignore all others.
    							 |k is the number of nearest neighbors that are used when reconstructing the domain.
    							 |dataset_dir is the directory of the Ayla dataset.""".stripMargin

    def parseFilterFlag(arg: String) = arg.toLowerCase match {
      case "-w" => Success((filterSet: Set[String]) => filterSet(_))
      case "-b" => Success((filterSet: Set[String]) => (s: String) => !filterSet(s))
      case _ => Failure(s"Error: Invalid filter flag.  Expected -w or -b, but found $arg.")
    }

    def parseFilterFile(arg: String) = new File(arg) match {
      case filterFile if filterFile.exists =>
        catchException(Source.fromFile(filterFile).getLines.toSet, s"An error occurred while trying to read ${filterFile.getAbsolutePath}")

      case f @ _ => Failure(s"Error: The whitelist/blacklist file ${f.getAbsolutePath} does not exist.")
    }

    def getFilter(argPair: Option[(String, String)]) = argPair match {
      case Some((filterFlagArg, filterFileArg)) =>
        for {
          filterMaker <- parseFilterFlag(filterFlagArg)
          filterSet <- parseFilterFile(filterFileArg)
        } yield filterMaker(filterSet)
      case None => Success((_: String) => true)
    }

    import scalaz.Scalaz._

    def getK(arg: String) = arg.parseInt.fail.map(
      _ => s"Error: Unable to parse parameter 'k'.  Expected a positive integer > 1 but found $arg instead.").validation.flatMap {
        case k if k > 1 => Success(k)
        case k @ _ => Failure("Error: k must be greater than 1.")
      }

    def getDatasetDir(arg: String) = new File(arg) match {
      case f if f.exists => Success(f)
      case f @ _ => Failure(s"Error: The specified dataset directory '${f.getAbsolutePath}' does not exist.")
    }

    def getFilterArgs = args match {
      case Array(filterFlagArg, filterFileArg, kArg, datasetDirArg, outputFunctionName) => Success(Some(filterFlagArg, filterFileArg))
      case Array(kArg, datasetDirArg) => Success(None)
      case _ => Failure(usage)
    }

    def splitArgs = args match {
      case Array(filterFlagArg, filterFileArg, kArg, datasetDirArg, outputFunctionName) => Success((Some(filterFlagArg, filterFileArg), kArg, datasetDirArg, outputFunctionName))
      case Array(kArg, datasetDirArg, outputFunctionName) => Success((None, kArg, datasetDirArg, outputFunctionName))
      case _ => Failure(usage)
    }

    for {
      (filterArgsOpt, kArg, datasetDirArg, outputFunctionName) <- splitArgs
      filter <- getFilter(filterArgsOpt)
      k <- getK(kArg)
      datasetDir <- getDatasetDir(datasetDirArg)
      outputFile <- catchException({
        val collabProjectsDir = new File(datasetDir, "collab_projects")
        if (!collabProjectsDir.exists)
          collabProjectsDir.mkdirs()
        new File(collabProjectsDir, outputFunctionName)
      })
    } yield {
      (filter, k, datasetDir, outputFile)
    }
  }

  def buildDomain(filter: Filter, k: Int, datasetDir: File, outputFile: File) = {
    for {
      dataset <- CachedDataset(datasetDir)
    } yield {
      val sampledIndices = dataset.pdbStreamProvider.zipWithIndex.flatMap {
        case (pdbPath, i) if filter(pdbPath) => Some(i)
        case _ => None
      }.toArray

      val sampledOrigPts = sampledIndices.map(dataset.origPoints)
      val knnFinder = new NearestNeighborSearcher(sampledOrigPts)
      val progress = new AtomicInteger()
      val faces = Timing("building knn graph") {
        sampledOrigPts.indices.par.flatMap { i =>
          val prog = progress.incrementAndGet
	        if (prog % 1000 == 0) {
	          println("Progress:  %.2f".format(100 * prog / sampledOrigPts.length.toDouble) + "%")
	        }
          knnFinder.getNearestNeighbors(i, k).map(j => (math.min(i, j), math.max(i, j)))
        }.distinct.map(pair => Array(pair._1, pair._2)).seq.toArray
      }
      val sc = new SimplicialComplex(sampledIndices.map(dataset.origPoints), faces)
      println("Betti zero:  " + sc.bettiZero)
      require(sc.bettiZero == 1, "Error:  The knn graph has multiple connected components (" + sc.bettiZero + " total).  Try using a larger value for k.")
      
      // Save faces and sampled indices
      val os = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(outputFile)))
      os.writeInt(faces.length)
      faces.foreach{face => 
        os.writeInt(face(0))
        os.writeInt(face(1))
      }
      os.writeInt(sampledIndices.length)
      sampledIndices.foreach(os.writeInt)
      os.flush
      os.close
    }
  }
  
  def loadDomain(sampledDomainFile: File, dataset: CachedDataset): (SimplicialComplex, Array[Int]) = {
    val is = new DataInputStream(new BufferedInputStream(new FileInputStream(sampledDomainFile)))
    val numFaces = is.readInt
    val faces = (0 until numFaces).map{_ =>
      Array(is.readInt, is.readInt)
    }.toArray
    val numSampledIndices = is.readInt
    val sampledIndices = (0 until numSampledIndices).map(_ => is.readInt).toArray
    is.close
    
    val sc = new SimplicialComplex(sampledIndices.map(dataset.origPoints).toArray, faces)
    (sc, sampledIndices)
  }
  
}
