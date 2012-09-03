package temp

//import java.io._
//import edu.osu.compgeom.util.IO._
//import edu.osu.compgeom.dataset.CachedDataset
//import edu.osu.compgeom.dataset.ZipFilePdbStreamProvider
//import edu.osu.compgeom.dataset.preprocess.KnnDatabase
//import colormap.Colormaps
//
//    import scalala.library.Plotting._
//    import scalala.tensor.dense.DenseVector
//    import java.awt.Color
//    import scalala.library.plotting.GradientPaintScale

object InestigateSurvivinNeighbors {

  def main(args: Array[String]): Unit = {
//    val datasetDir = new File("/home/harveywi/research/ScalaCrystals/Survivin_K5")
//    val pcd = withObjectInputStream(new File(datasetDir, "pcd_v2.dat")){_.readObject.asInstanceOf[Array[Array[Float]]]}
//    
//    val funcVals = withBufferedReader(new File(datasetDir, "scalar_functions/potential_energy.txt")) { br =>
//      Iterator.continually(br.readLine).takeWhile(_ != null).drop(1).map(_.toFloat).toArray
//    }
//    val minFuncVal = funcVals.min
//    val rangeFuncVal = funcVals.max - minFuncVal
//    
//    val x = DenseVector(pcd.map(_(0)))
//    val y = DenseVector(pcd.map(_(1)))
//    val s = DenseVector.fill(pcd.size)(.005)
//    
//    figure(1)
//    
//    val literalColors = funcVals.zipWithIndex.map { case (f, i) => i -> Colormaps.getColor(f, minFuncVal, rangeFuncVal, Colormaps.CmapType.JET) }.toMap
//    
//    scatter(x, y, s, literalColors)
    
  }
//  def main(args: Array[String]): Unit = {
//    val datasetDir = new File("/home/harveywi/research/ScalaCrystals/Survivin_252996")
//    val dataset = new CachedDataset(datasetDir) with ZipFilePdbStreamProvider
//
//    val knnDatabase = new KnnDatabase(new File(datasetDir, "survivin_neighbors"), dataset)
//
//    println("Old neighbors:  " + dataset.neighbors(0).take(20).mkString("", ",", "\n"))
//    knnDatabase.withNearestNeighborUnpacker(0) { it =>
//      println("New neighbors:  " + it.take(20).mkString("", ",", "\n"))
//    }
//
//    val funcValsRaw = withBufferedReader(new File(datasetDir, "scalar_functions/potential_energy.txt")) { br =>
//      Iterator.continually(br.readLine).takeWhile(_ != null).drop(1).map(_.toFloat).toArray
//    }
//
//    val subset = funcValsRaw.zipWithIndex.sortBy(_._1).reverse.drop(20000).take(1000).map(_._2)
//    val funcVals = subset.map(funcValsRaw)
//
//    import scalala.library.Plotting._
//    import scalala.tensor.dense.DenseVector
//    import java.awt.Color
//    import scalala.library.plotting.GradientPaintScale
//
//    val x = DenseVector(subset.map(i => dataset.pcaPoints(i)).map(_(0)))
//    val y = DenseVector(subset.map(i => dataset.pcaPoints(i)).map(_(1)))
//    val s = DenseVector.fill(subset.size)(5)
//
//    //    val labels: PartialFunction[Int, String] =
//    //      Map(1 -> "Red", 4 -> "Blue")
//    //
//    //    val tips: PartialFunction[Int, String] =
//    //      { case i: Int => i.toString }
//
//    figure(1)
//
//    val minFuncVal = funcValsRaw.min
//    val rangeFuncVal = funcValsRaw.max - minFuncVal
//
//    val literalColors = funcVals.zipWithIndex.map { case (f, i) => i -> Colormaps.getColor(f, minFuncVal, rangeFuncVal, Colormaps.CmapType.JET) }.toMap
//
//    //    val literalColors: PartialFunction[Int, Color] =
//    //      Map(1 -> Color.RED, 4 -> Color.BLUE) orElse { case x: Int => Color.WHITE }
//
//    scatter(x, y, s, literalColors)
//
//    hold(true)
//
//    (0 to 5).foreach { _ =>
//      val z = subset.indices.sortBy(_ => math.random).head
//
//      dataset.neighbors(subset(z)).take(10).foreach { i =>
//        val xs = DenseVector(dataset.pcaPoints(subset(z))(0), dataset.pcaPoints(i)(0))
//        val ys = DenseVector(dataset.pcaPoints(subset(z))(1), dataset.pcaPoints(i)(1))
//        plot(xs, ys, colorcode = "blue")
//      }
//
//      knnDatabase.withNearestNeighborUnpacker(subset(z)) { it =>
//        it.drop(1).take(10).foreach { i =>
//          val xs = DenseVector(dataset.pcaPoints(subset(z))(0), dataset.pcaPoints(i)(0))
//          val ys = DenseVector(dataset.pcaPoints(subset(z))(1), dataset.pcaPoints(i)(1))
//          plot(xs, ys, colorcode = "red")
//        }
//      }
//
//      // Show 3D nearest neighbors using green
//      def euclidDist(v1: Array[Float], v2: Array[Float]): Double = {
//        math.sqrt(v1.indices.iterator.map { j => val diff = v1(j) - v2(j); diff * diff }.sum)
//      }
//      dataset.pcaPoints.indices.map { i =>
//        val dist = euclidDist(dataset.pcaPoints(subset(z)), dataset.pcaPoints(i))
//        (i, dist)
//      }.sortBy(_._2).take(10).map(_._1).foreach { i =>
//        val xs = DenseVector(dataset.pcaPoints(subset(z))(0), dataset.pcaPoints(i)(0))
//        val ys = DenseVector(dataset.pcaPoints(subset(z))(1), dataset.pcaPoints(i)(1))
//        plot(xs, ys, colorcode = "green")
//      }
//    }
//
//    //    figure(2)
//    //
//    //    val c = DenseVector.rand(10);
//    //
//    //    val paintScale = GradientPaintScale(0.0, 1.0)
//    //
//    //    val scaleColors = Map() ++ (0 until c.length).map(i => (i, paintScale(c(i))));
//    //
//    //    scatter(x, y, s, scaleColors, labels = labels, tips = tips)
//
//  }

}