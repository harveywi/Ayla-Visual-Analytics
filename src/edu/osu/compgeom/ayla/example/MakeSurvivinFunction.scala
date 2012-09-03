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
import edu.osu.compgeom.dataset.preprocess.NearestNeighborSearcher

object MakeSurvivinFunction {

//  def main(args: Array[String]): Unit = {
//    val dir = new File("/home/harveywi/research/ScalaCrystals/Survivin_252996/")
//    val dataset = new CachedDataset(dir) with FilePdbStreamProvider
//
//    // Choose some subset of the data that we are interested in.  Here, let's select only the low-energy conformations.
//    val potentialEnergies = withBufferedReader(new File(dir, "scalar_functions/potential_energy.txt")) { br =>
//      Iterator.continually(br.readLine).takeWhile(_ != null).drop(1).map(_.toFloat).toArray
//    }
//
//    val numToSample = 20000
//    val k = 15
//    val lowEnergyConformationIDs = potentialEnergies.zipWithIndex.sortWith((pair1, pair2) => pair1._1 < pair2._1).take(numToSample).map(_._2)
//    val unsampledToSampled = lowEnergyConformationIDs.zipWithIndex.toMap
//
//    val vertices = lowEnergyConformationIDs.map(dataset.pcaPoints)
//    val s = lowEnergyConformationIDs.toSet
//
//    //val knnDatabase = new KnnDatabase(new File("/home/harveywi/research/ScalaCrystals/Survivin_252996/survivin_neighbors"), dataset)
//    val knnFinder = new NearestNeighborSearcher(dataset.origPoints)
//    
//    val faces = Timing("knn") {
//      lowEnergyConformationIDs.par.flatMap { i =>
//        val sampledI = unsampledToSampled(i)
//        knnFinder.getNearestNeighbors(i, k)
////        knnDatabase.withNearestNeighborUnpacker(i) { unpacker =>
////          unpacker.drop(1).filter(s.contains).map(unsampledToSampled).take(k).map(j => (math.min(sampledI, j), math.max(sampledI, j))).toArray
////        }
//      }.distinct.map(pair => Array(pair._1, pair._2)).seq.toArray
//    }
//
//    val sc = new SimplicialComplex(vertices, faces)
//    println("Betti zero:  " + sc.bettiZero)
//
//    val funcVals = lowEnergyConformationIDs.map(potentialEnergies)
//
//    val sf = new ScalarFunction(vertices, faces, funcVals)
//
//    val outFile = new File(dir, "collab_projects/20000_k15_low_e.dat")
//    withObjectOutputStream(outFile) { oos =>
//      val collab = new AylaCollaborationProject(outFile, sf, lowEnergyConformationIDs)
//      collab.name = "20K low-energy, k=15"
//      collab.description =
//        """|20,000 lowest-energy conformations sampled from the survivin dataset.  15 nearest neighbors
//        	 |were used to compute the neighborhood graph.
//        	 |""".stripMargin
//      oos.writeObject(collab)
//    }
//  }
}