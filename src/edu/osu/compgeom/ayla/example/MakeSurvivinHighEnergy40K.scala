package edu.osu.compgeom.ayla.example

import java.io.File
import edu.osu.compgeom.dataset.CachedDataset
import edu.osu.compgeom.util.IO._
import edu.osu.compgeom.topology.{ SimplicialComplex, ScalarFunction }
import edu.osu.compgeom.dataset.preprocess.KnnDatabase
import edu.osu.compgeom.util.Timing
import edu.osu.compgeom.landscapes.BSPTreeEnsemble
import edu.osu.compgeom.ct.ContourTree
import edu.osu.compgeom.landscapes.floorplan.VoronoiFloorplan
import edu.osu.compgeom.ayla.AylaCollaborationProject
import edu.osu.compgeom.dataset.FilePdbStreamProvider

object MakeSurvivinHighEnergy40K {
//
//  def main(args: Array[String]): Unit = {
//    val dir = new File("/home/harveywi/research/ScalaCrystals/Survivin_252996/")
//    val dataset = new CachedDataset(dir) with FilePdbStreamProvider
//
//    // Choose some subset of the data that we are interested in.  Here, let's select only the low-energy conformations.
//    val potentialEnergies = withBufferedReader(new File(dir, "scalar_functions/potential_energy.txt")) { br =>
//      Iterator.continually(br.readLine).takeWhile(_ != null).drop(1).map(_.toFloat).toArray
//    }
//
//    val numToSample = 40000
//    val k = 35
//    val highEnergyConformationIDs = potentialEnergies.zipWithIndex.sortWith((pair1, pair2) => pair1._1 > pair2._1).take(numToSample).map(_._2)
//    val unsampledToSampled = highEnergyConformationIDs.zipWithIndex.toMap
//
//    val vertices = highEnergyConformationIDs.map(dataset.pcaPoints)
//    val s = highEnergyConformationIDs.toSet
//    
//    val knnDatabase = new KnnDatabase(new File("/home/harveywi/research/ScalaCrystals/Survivin_252996/survivin_neighbors"), dataset)
//
//    val faces = Timing("knn") {
//      highEnergyConformationIDs.par.flatMap { i =>
//        val sampledI = unsampledToSampled(i)
//        knnDatabase.withNearestNeighborUnpacker(i) { unpacker =>
//          unpacker.filter(s.contains).map(unsampledToSampled).take(k).map(j => (math.min(sampledI, j), math.max(sampledI, j))).toArray
//        }
//      }.distinct.map(pair => Array(pair._1, pair._2)).seq.toArray
//    }
//
//    val sc = new SimplicialComplex(vertices, faces)
//    println("Betti zero:  " + sc.bettiZero)
//    if (sc.bettiZero != 1) {
//      System.err.println("Only continuing if Betti zero == 1.  Exiting.")
//      System.exit(0)
//    }
//
//    val funcVals = highEnergyConformationIDs.map(potentialEnergies)
//
//    val sf = new ScalarFunction(vertices, faces, funcVals)
//
//    val outFile = new File(dir, "collab_projects/40000_k15_high_e.dat")
//    withObjectOutputStream(outFile) { oos =>
//      val collab = new AylaCollaborationProject(outFile, sf, highEnergyConformationIDs)
//      collab.name = "40K high-energy, k=15"
//      collab.description =
//        """|%d high-energy conformations sampled from the survivin dataset.  %d nearest neighbors
//        	 |were used to compute the neighborhood graph.
//        	 |""".stripMargin.format(numToSample, k) 
//      oos.writeObject(collab)
//    }
//  }
//
//  def estimateArea(vertIndices: Array[Int], useCountForAreaEstimation: Boolean, pcdCoords: Array[Array[Float]]): Double = {
//    if (useCountForAreaEstimation)
//      return vertIndices.size
//
//    def calcMean(): Array[Double] = {
//      val nDims = pcdCoords(0).size
//      val mean = new Array[Double](nDims)
//      vertIndices.foreach { idx =>
//        val p = pcdCoords(idx)
//        (0 until nDims).foreach { i =>
//          mean(i) += p(i)
//        }
//      }
//      val n = vertIndices.size.toDouble
//      (0 until nDims).foreach { i => mean(i) /= n }
//      mean
//    }
//
//    def calcDistSum(mean: Array[Double]): Double = {
//      var distSum = 0d
//      vertIndices.foreach { idx =>
//        var sumSq = 0d
//        val p = pcdCoords(idx)
//        (0 until mean.size).foreach { i =>
//          val diff = p(i) - mean(i)
//          sumSq += diff * diff
//        }
//        distSum += math.sqrt(sumSq)
//      }
//      distSum
//    }
//
//    // Compute standard deviation.
//    val mean = calcMean()
//    val distSum = calcDistSum(mean)
//
//    val stdDev = distSum / (vertIndices.size - 1).toDouble
//    if (stdDev <= 0) {
//      return .1
//    }
//    return stdDev
//  }

}