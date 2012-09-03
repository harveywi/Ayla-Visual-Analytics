package edu.osu.compgeom.ayla.dsl

import java.io._
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
import edu.osu.compgeom.dataset.ZipFilePdbStreamProvider
import edu.osu.compgeom.dataset.preprocess.NearestNeighborSearcher
import java.util.PriorityQueue
import java.util.PriorityQueue

/**
 * Dataset directory:  File
 *
 * Knn database:  File
 *
 * Function values:  File
 *
 * Neighborhood:  Take k, or increase k while disconnected.
 *
 * Sample size:  Int
 *
 * Filter rule:  Performs any necessary transformations on a conformation, and return true iff the conformation should
 * be sampled.
 *
 * @author harveywi
 *
 */
object AylaDSL {

  /*
   *     val sampleSet = FROM DATASET datasetDir SAMPLE sampler
    val knnGraph = FROM SAMPLE sampleSet WITH_KNN_DATABASE knnDir MAKE_KNN_GRAPH k
    val scalarFunction = FROM KNN_GRAPH knnGraph WITH_FUNCTION_VALUES funcVals MAKE_SCALAR_FUNCTION;
    TO FILE outFile WRITE_SCALAR_FUNCTION scalarFunction WITH_NAME outFunctionName WITH_DESCRIPTION outDescription
   */

  //  object FROM {
  //    def DATASET(datasetDir: File) = new {
  //      val dataset = new CachedDataset(datasetDir) with FilePdbStreamProvider
  //      def SAMPLE(conformationFilter: CachedDataset => Array[Int]): SampledDataset = {
  //        val sampledConformations = conformationFilter(dataset)
  //        new SampledDataset(dataset, sampledConformations)
  //      }
  //    }
  //
  //    def SAMPLE(sampledDataset: SampledDataset) = sampledDataset
  //  }
  //
  //  class SampledDataset(val dataset: CachedDataset, val sampledConformations: Array[Int]) {
  //    val unsampledToSampled = sampledConformations.zipWithIndex.toMap
  //    val vertices = sampledConformations.map(dataset.pcaPoints)
  //    val sampledConformationsSet = sampledConformations.toSet
  //    def MAKE_KNN_GRAPH(k: Int): KnnGraph = {
  //      val faces = Timing("building knn graph") {
  //        val knnFinder = new NearestNeighborSearcher(dataset.origPoints)
  //        sampledConformations.par.flatMap { i =>
  //          val sampledI = unsampledToSampled(i)
  //          val neighbors = knnFinder.getNearestNeighbors(i)
  ////          val packedNeighbors = knnDatabase.withNearestNeighborUnpacker(i) { unpacker =>
  ////            unpacker.drop(1).filter(sampledConformationsSet.contains).map(unsampledToSampled).take(k).map(j => (math.min(sampledI, j), math.max(sampledI, j))).toArray
  ////          }
  //          packedNeighbors
  //        }.distinct.map(pair => Array(pair._1, pair._2)).seq.toArray
  //      }
  //
  //      val sc = new SimplicialComplex(vertices, faces)
  //      println("Betti zero:  " + sc.bettiZero)
  //      require(sc.bettiZero == 1, "Error:  The knn graph has multiple connected components (" + sc.bettiZero + " total).  Try using a larger value for k.")
  //      new KnnGraph(SampledDataset.this, sc)
  //    }
  //  }
  //  
  //  class SampledScalarFunction(val knnGraph: KnnGraph, val sf: ScalarFunction)
  //
  //  class KnnGraph(val sampledDataset: SampledDataset, val sc: SimplicialComplex) {
  //    def WITH_FUNCTION_VALUES(unsampledFuncVals: Array[Float]) = new {
  //      def MAKE_SCALAR_FUNCTION(): SampledScalarFunction = {
  //        val sampledFuncVals = sampledDataset.sampledConformations.map(unsampledFuncVals)
  //        val sf = new ScalarFunction(sc, sampledFuncVals)
  //        new SampledScalarFunction(KnnGraph.this, sf)
  //      }
  //    }
  //  }
  //
  //  object TO {
  //    def FILE(outFile: File) = new {
  //      def WRITE_SCALAR_FUNCTION(sampledScalarFunction: SampledScalarFunction) = new {
  //        def WITH_NAME(name: String) = new {
  //          def WITH_DESCRIPTION(desc: String): Unit = {
  //            withObjectOutputStream(outFile) { oos =>
  //              val collab = new AylaCollaborationProject(outFile, sampledScalarFunction.sf, sampledScalarFunction.knnGraph.sampledDataset.sampledConformations)
  //              collab.name = name
  //              collab.description = desc
  //              oos.writeObject(collab)
  //            }
  //          }
  //        }
  //      }
  //    }
  //  }

  object FROM {
    object DATASET {
      def apply(file: File) = new {
        val dataset = new CachedDataset(file) with ZipFilePdbStreamProvider
        dataset.loadOrigPoints()
        object SAMPLE {
          def apply(sampler: CachedDataset => Array[Int]) = new SampledDataset(dataset, sampler(dataset))
        }
      }
    }

    object SAMPLESET {
      def apply(sd: SampledDataset) = new {
        object MAKE_KNN_GRAPH {
          def apply(k: Int): KNearestNeighborGraph = {
            val sampledOrigPts = sd.sampledIndices.map(sd.dataset.origPoints).map(_.take(16))
            val knnFinder = new NearestNeighborSearcher(sampledOrigPts)
            val faces = Timing("building knn graph") {
              sampledOrigPts.indices.par.flatMap { i =>
                knnFinder.getNearestNeighbors(i, k).map(j => (math.min(i, j), math.max(i, j)))
              }.distinct.map(pair => Array(pair._1, pair._2)).seq.toArray
            }
            val sc = new SimplicialComplex(sd.sampledIndices.map(sd.dataset.pcaPoints), faces)
		        println("Betti zero:  " + sc.bettiZero)
		        require(sc.bettiZero == 1, "Error:  The knn graph has multiple connected components (" + sc.bettiZero + " total).  Try using a larger value for k.")
            val knnGraph = new KNearestNeighborGraph(sc, sd.sampledIndices)
            knnGraph
          }
        }
      }
    }

    def KNN_GRAPH(knnGraph: KNearestNeighborGraph) = knnGraph
  }

  class SampledDataset(val dataset: CachedDataset, val sampledIndices: Array[Int])

  class KNearestNeighborGraph(sc: SimplicialComplex, val sampledToUnsampled: Array[Int]) {
    object WITH_FUNCTION_VALUES {
      def apply(funcVals: Array[Float]) = new {
        def MAKE_SCALAR_FUNCTION = {
          val sampledFuncVals = sampledToUnsampled.map(funcVals)
          val sf = new ScalarFunction(sc, sampledFuncVals)
          new SampledScalarFunction(KNearestNeighborGraph.this, sf)
        }
      }
    }
  }
  
  class SampledScalarFunction(val knnGraph: KNearestNeighborGraph, val sf: ScalarFunction)

  object TO {
    def FILE(outFile: File) = new {
      def WRITE_SCALAR_FUNCTION(sampledScalarFunction: SampledScalarFunction) = new {
        def WITH_NAME(name: String) = new {
          def WITH_DESCRIPTION(desc: String): Unit = {
            withObjectOutputStream(outFile) { oos =>
              val collab = new AylaCollaborationProject(outFile, sampledScalarFunction.sf, sampledScalarFunction.knnGraph.sampledToUnsampled)
              collab.name = name
              collab.description = desc
              oos.writeObject(collab)
            }
          }
        }
      }
    }
  }

  def loadFunctionVals(funcValsFile: File) = withBufferedReader(funcValsFile) { br =>
    Iterator.continually(br.readLine).takeWhile(_ != null).drop(1).map(_.toFloat).toArray
  }

  def main(args: Array[String]): Unit = {
    val dir = new File("/home/harveywi/research/ScalaCrystals/Survivin_F93_F101")

    val funcVals = loadFunctionVals(new File(dir, "scalar_functions/potential_energy.txt"))

    val conformationFilter: (CachedDataset) => Array[Int] = { dataset =>
      dataset.pcaPoints.indices.zip(funcVals).sortBy(_._2).take(252996).map(_._1).toArray
    }
    
    val k = 19
    
    val outFile = new File(dir, "collab_projects/testing_k8.dat")
    val outFunctionName = "Whole dataset, k=8"
    val outDescription = "Just a test."

    val sampleSet = FROM DATASET dir SAMPLE conformationFilter
    val knnGraph = FROM SAMPLESET sampleSet MAKE_KNN_GRAPH k
    val scalarFunction = FROM KNN_GRAPH knnGraph WITH_FUNCTION_VALUES funcVals MAKE_SCALAR_FUNCTION;
        TO FILE outFile WRITE_SCALAR_FUNCTION scalarFunction WITH_NAME outFunctionName WITH_DESCRIPTION outDescription
  }

  /*
  object FROM {
    def DATASET(datasetDir: File) = new UnsampledDataset(new CachedDataset(datasetDir) with FilePdbStreamProvider)
    def SAMPLE(sampledDataset: SampledDataset) = sampledDataset
    def KNN_GRAPH(knnGraph: KnnGraph) = knnGraph
  }

  class UnsampledDataset(dataset: CachedDataset) {
    def SAMPLE(conformationFilter: CachedDataset => Array[Int]): SampledDataset = {
      val sampledConformations = conformationFilter(dataset)
      new SampledDataset(dataset, sampledConformations)
    }
  }

  class SampledDataset(val dataset: CachedDataset, val sampledConformations: Array[Int]) {
    val unsampledToSampled = sampledConformations.zipWithIndex.toMap
    val vertices = sampledConformations.map(dataset.pcaPoints)
    val sampledConformationsSet = sampledConformations.toSet

    def WITH_KNN_DATABASE(knnDir: File) = new KnnBuilder(new KnnDatabase(knnDir, dataset))

    class KnnBuilder(knnDatabase: KnnDatabase) {
      def MAKE_KNN_GRAPH(k: Int): KnnGraph = {
        val faces = Timing("building knn graph") {
          sampledConformations.par.flatMap { i =>
            val sampledI = unsampledToSampled(i)
            val packedNeighbors = knnDatabase.withNearestNeighborUnpacker(i) { unpacker =>
              unpacker.drop(1).filter(sampledConformationsSet.contains).map(unsampledToSampled).take(k).map(j => (math.min(sampledI, j), math.max(sampledI, j))).toArray
            }
            packedNeighbors
          }.distinct.map(pair => Array(pair._1, pair._2)).seq.toArray
        }

        val sc = new SimplicialComplex(vertices, faces)
        println("Betti zero:  " + sc.bettiZero)
        require(sc.bettiZero == 1, "Error:  The knn graph has multiple connected components (" + sc.bettiZero + " total).  Try using a larger value for k.")
        new KnnGraph(SampledDataset.this, sc)
      }
    }
  }

  class SampledScalarFunction(val knnGraph: KnnGraph, val sf: ScalarFunction)

  class KnnGraph(val sampledDataset: SampledDataset, val sc: SimplicialComplex) {
    def WITH_FUNCTION_VALUES(unsampledFuncVals: Array[Float]) = new {
      def MAKE_SCALAR_FUNCTION(): SampledScalarFunction = {
        val sampledFuncVals = sampledDataset.sampledConformations.map(unsampledFuncVals)
        val sf = new ScalarFunction(sc, sampledFuncVals)
        new SampledScalarFunction(KnnGraph.this, sf)
      }
    }
  }

  object TO {
    def FILE(outFile: File) = new {
      def WRITE_SCALAR_FUNCTION(sampledScalarFunction: SampledScalarFunction) = new {
        def WITH_NAME(name: String) = new {
          def WITH_DESCRIPTION(desc: String): Unit = {
            withObjectOutputStream(outFile) { oos =>
              val collab = new AylaCollaborationProject(outFile, sampledScalarFunction.sf, sampledScalarFunction.knnGraph.sampledDataset.sampledConformations)
              collab.name = name
              collab.description = desc
              oos.writeObject(collab)
            }
          }
        }
      }
    }
  }

  def loadFunctionVals(funcValsFile: File) = withBufferedReader(funcValsFile) { br =>
    Iterator.continually(br.readLine).takeWhile(_ != null).drop(1).map(_.toFloat).toArray
  }

  def takeLowEnergySurvivin(numToSample: Int, k: Int) = {
    // Input
    val datasetDir = new File("/home/harveywi/research/ScalaCrystals/Survivin_252996")
    val knnDir = new File(datasetDir, "survivin_neighbors")
    val funcVals = loadFunctionVals(new File(datasetDir, "scalar_functions/potential_energy.txt"))

    // Output spec
    val outFile = new File(datasetDir, "collab_projects/%d_k%d_low_e.dat".format(numToSample, k))
    val outFunctionName = "%dK low-energy, k=%d".format((numToSample / 1000).toInt, k)
    val outDescription =
      """|%d lowest-energy conformations sampled from the survivin dataset.  %d nearest neighbors
        	 |were used to compute the neighborhood graph.
        	 |""".stripMargin.format(numToSample, k)

    // How to process the dataset.
    val sampler: (CachedDataset => Array[Int]) = { dataset =>
      funcVals.zipWithIndex.sortWith((pair1, pair2) => pair1._1 < pair2._1).take(numToSample).map(_._2)
    }

    val sampleSet = FROM DATASET datasetDir SAMPLE sampler
    val knnGraph = FROM SAMPLE sampleSet WITH_KNN_DATABASE knnDir MAKE_KNN_GRAPH k
    val scalarFunction = FROM KNN_GRAPH knnGraph WITH_FUNCTION_VALUES funcVals MAKE_SCALAR_FUNCTION;
    TO FILE outFile WRITE_SCALAR_FUNCTION scalarFunction WITH_NAME outFunctionName WITH_DESCRIPTION outDescription
  }

  def takeAllSurvivin(k: Int) = {
    // Input
    val datasetDir = new File("/home/harveywi/research/ScalaCrystals/Survivin_252996")
    val knnDir = new File(datasetDir, "survivin_neighbors")
    val funcVals = loadFunctionVals(new File(datasetDir, "scalar_functions/potential_energy.txt"))

    // Output spec
    val outFile = new File(datasetDir, "collab_projects/252996_k%d_pe.dat".format(k))
    val outFunctionName = "Entire Survivin Dataset (k=%d)".format(k)
    val outDescription =
      """|The entire survivin dataset.  Function value is potential energy.""".stripMargin

    // How to process the dataset.
    val sampler: (CachedDataset => Array[Int]) = { dataset =>
      dataset.origPoints.indices.toArray
    }

    val sampleSet = FROM DATASET datasetDir SAMPLE sampler
    val knnGraph = FROM SAMPLE sampleSet WITH_KNN_DATABASE knnDir MAKE_KNN_GRAPH k
    val scalarFunction = FROM KNN_GRAPH knnGraph WITH_FUNCTION_VALUES funcVals MAKE_SCALAR_FUNCTION;
    TO FILE outFile WRITE_SCALAR_FUNCTION scalarFunction WITH_NAME outFunctionName WITH_DESCRIPTION outDescription
  }

  def takeAllGammaSCrystallin(k: Int) = {
    // Input
    val datasetDir = new File("/home/harveywi/research/ScalaCrystals/GammaSCrystallin")
    val knnDir = new File("/home/harveywi/research/cluster/media/hd/gammaSCrystallin_neighbors_v2")
    val funcVals = loadFunctionVals(new File(datasetDir, "scalar_functions/potential_energy.txt"))

    // Output spec
    val outFile = new File(datasetDir, "collab_projects/n%d_k%d_pe.dat".format(funcVals.size, k))
    val outFunctionName = "Entire GammaS-Crystallin Dataset (k=%d)".format(k)
    val outDescription =
      """|The entire GammaS-Crystallin dataset.  Function value is potential energy.""".stripMargin

    // How to process the dataset.
    val sampler: (CachedDataset => Array[Int]) = { dataset =>
      dataset.origPoints.indices.toArray
    }

    val sampleSet = FROM DATASET datasetDir SAMPLE sampler
    val knnGraph = FROM SAMPLE sampleSet WITH_KNN_DATABASE knnDir MAKE_KNN_GRAPH k
    val scalarFunction = FROM KNN_GRAPH knnGraph WITH_FUNCTION_VALUES funcVals MAKE_SCALAR_FUNCTION;
    TO FILE outFile WRITE_SCALAR_FUNCTION scalarFunction WITH_NAME outFunctionName WITH_DESCRIPTION outDescription
  }

  def takeLowEnergyGammaSCrystallin(k: Int) = {
    // Input
    val datasetDir = new File("/home/harveywi/research/ScalaCrystals/GammaSCrystallin")
    val knnDir = new File("/home/harveywi/research/cluster/media/hd/gammaSCrystallin_neighbors_v2")
    val funcVals = loadFunctionVals(new File(datasetDir, "scalar_functions/potential_energy.txt"))

    val numToSample = funcVals.size - 1000

    // Output spec
    val outFile = new File(datasetDir, "collab_projects/%d_k%d_low_e.dat".format(numToSample, k))
    val outFunctionName = "%dK low-energy, k=%d".format((numToSample / 1000).toInt, k)
    val outDescription =
      """|%d lowest-energy conformations sampled from the GammaS-Crystallin dataset.  %d nearest neighbors
        	 |were used to compute the neighborhood graph.
        	 |""".stripMargin.format(numToSample, k)

    // How to process the dataset.
    val sampler: (CachedDataset => Array[Int]) = { dataset =>
      funcVals.zipWithIndex.sortWith((pair1, pair2) => pair1._1 < pair2._1).take(numToSample).map(_._2)
    }

    val sampleSet = FROM DATASET datasetDir SAMPLE sampler
    val knnGraph = FROM SAMPLE sampleSet WITH_KNN_DATABASE knnDir MAKE_KNN_GRAPH k
    val scalarFunction = FROM KNN_GRAPH knnGraph WITH_FUNCTION_VALUES funcVals MAKE_SCALAR_FUNCTION;
    TO FILE outFile WRITE_SCALAR_FUNCTION scalarFunction WITH_NAME outFunctionName WITH_DESCRIPTION outDescription
  }

  def takeSurvivinK5 {
    val datasetDir = new File("/home/harveywi/research/ScalaCrystals/Survivin_K5")
    val funcVals = loadFunctionVals(new File(datasetDir, "scalar_functions/potential_energy.txt"))
    val k = 19
    val outFile = new File(datasetDir, "collab_projects/wholeDataset_k%d.dat".format(k))
    val outFunctionName = "Whole Dataset k=%d".format(k)
    val outDescription = "Whole dataset."

    val dataset = new CachedDataset(datasetDir) with ZipFilePdbStreamProvider

    val nns = new NearestNeighborSearcher(dataset.origPoints)

    val edgesWithDupes = (0 until dataset.pcaPoints.size).par.flatMap(i =>
      nns.getNearestNeighbors(i, k + 1).drop(1).map(j => Tuple2(math.min(i, j), math.max(i, j)))).toArray

    val edges = edgesWithDupes.distinct.filter { case (i, j) => i != j }.map { case (i, j) => Array(i, j) }

    val sf = new ScalarFunction(dataset.pcaPoints, edges, funcVals)

    withObjectOutputStream(outFile) { oos =>
      val collab = new AylaCollaborationProject(outFile, sf, dataset.pcaPoints.indices.toArray)
      collab.name = outFunctionName
      collab.description = outDescription
      oos.writeObject(collab)
    }
  }

  def takeSurvivin3D {
    val datasetDir = new File("/home/harveywi/research/ScalaCrystals/Survivin_3D")
    val funcVals = loadFunctionVals(new File(datasetDir, "scalar_functions/potential_energy.txt"))
    val k = 19
    val outFile = new File(datasetDir, "collab_projects/wholeDataset_k%d.dat".format(k))
    val outFunctionName = "Whole Dataset k=%d".format(k)
    val outDescription = "Whole dataset."

    val dataset = new CachedDataset(datasetDir) with ZipFilePdbStreamProvider

    //    val nns = new NearestNeighborSearcher(dataset.origPoints)

    class NearNeighborFinder {
      class DistPair(val id: Int, val distSq: Double) extends Comparable[DistPair] {
        override def compareTo(o: DistPair) = java.lang.Double.compare(o.distSq, distSq)
      }
      def getNearestNeighbors(i: Int): Array[Int] = {
        val p = dataset.pcaPoints(i)
        val pq = new PriorityQueue[DistPair]

        dataset.pcaPoints.indices.foreach { j =>
          val minDist = if (pq.size() < k + 1) Double.MaxValue else pq.peek().distSq
          val q = dataset.pcaPoints(j)
          val dx = p(0) - q(0)
          val dy = p(1) - q(1)
          val dz = p(2) - q(2)
          val distSq = dx * dx + dy * dy + dz * dz
          if (distSq < minDist) {
            pq.add(new DistPair(j, distSq))
            if (pq.size > k + 1)
              pq.poll()
          }
        }
        val ret = new Array[Int](k + 1)
        var idx = k;
        while (idx >= 0) {
          ret(idx) = pq.poll().id
          idx -= 1
        }
        ret
      }
    }

    val nns = new NearNeighborFinder

    val edgesWithDupes = (0 until dataset.pcaPoints.size).par.flatMap { i =>
      val neighbors = nns.getNearestNeighbors(i)
      neighbors.map(j => Tuple2(math.min(i, j), math.max(i, j)))
    }.toArray

    val edges = edgesWithDupes.distinct.filter { case (i, j) => i != j }.map { case (i, j) => Array(i, j) }

    val sf = new ScalarFunction(dataset.pcaPoints, edges, funcVals)

    withObjectOutputStream(outFile) { oos =>
      val collab = new AylaCollaborationProject(outFile, sf, dataset.pcaPoints.indices.toArray)
      collab.name = outFunctionName
      collab.description = outDescription
      oos.writeObject(collab)
    }
  }

  def takeSurvivin1D {
    val datasetDir = new File("/home/harveywi/research/ScalaCrystals/Survivin_1D")
    val funcVals = loadFunctionVals(new File(datasetDir, "scalar_functions/potential_energy.txt"))
    val k = 19
    val outFile = new File(datasetDir, "collab_projects/wholeDataset_k%d.dat".format(k))
    val outFunctionName = "Whole Dataset k=%d".format(k)
    val outDescription = "Whole dataset."

    val dataset = new CachedDataset(datasetDir) with ZipFilePdbStreamProvider

    //    val nns = new NearestNeighborSearcher(dataset.origPoints)

    class NearNeighborFinder {
      class DistPair(val id: Int, val distSq: Double) extends Comparable[DistPair] {
        override def compareTo(o: DistPair) = java.lang.Double.compare(o.distSq, distSq)
      }
      def getNearestNeighbors(i: Int): Array[Int] = {
        val p = dataset.pcaPoints(i)
        val pq = new PriorityQueue[DistPair]

        dataset.pcaPoints.indices.foreach { j =>
          val minDist = if (pq.size() < k + 1) Double.MaxValue else pq.peek().distSq
          val q = dataset.pcaPoints(j)
          val dx = p(0) - q(0)
          val distSq = dx * dx
          if (distSq < minDist) {
            pq.add(new DistPair(j, distSq))
            if (pq.size > k + 1)
              pq.poll()
          }
        }
        val ret = new Array[Int](k + 1)
        var idx = k;
        while (idx >= 0) {
          ret(idx) = pq.poll().id
          idx -= 1
        }
        ret
      }
    }

    val nns = new NearNeighborFinder

    val edgesWithDupes = (0 until dataset.pcaPoints.size).par.flatMap { i =>
      val neighbors = nns.getNearestNeighbors(i)
      neighbors.map(j => Tuple2(math.min(i, j), math.max(i, j)))
    }.toArray

    val edges = edgesWithDupes.distinct.filter { case (i, j) => i != j }.map { case (i, j) => Array(i, j) }

    val sf = new ScalarFunction(dataset.pcaPoints, edges, funcVals)

    withObjectOutputStream(outFile) { oos =>
      val collab = new AylaCollaborationProject(outFile, sf, dataset.pcaPoints.indices.toArray)
      collab.name = outFunctionName
      collab.description = outDescription
      oos.writeObject(collab)
    }
  }

  def main(args: Array[String]): Unit = {
    //    takeAllGammaSCrystallin(24)
    //  	takeLowEnergyGammaSCrystallin(24)
    takeSurvivin1D
  }
  */
}