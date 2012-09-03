package edu.osu.compgeom.dataset.preprocess

import edu.osu.compgeom.dataset.CachedDataset
import java.io._
import scala.actors._
import scala.actors.Futures._
import java.util.zip.GZIPOutputStream
import scala.collection.mutable.BitSet
import java.awt.image.BufferedImage
import java.awt.Color
import javax.media.jai.JAI
import edu.osu.compgeom.util.IO._
import java.util.TreeMap
import edu.osu.compgeom.util.perm.Permutation
import edu.osu.compgeom.util.perm.LeftRightArray
import java.util.zip.GZIPInputStream
import edu.osu.compgeom.util.Timing
import edu.osu.compgeom.dataset._
//import jcuda._
//import jcuda.jcublas.JCublas
import no.uib.cipr.matrix.DenseMatrix
import org.netlib.blas.BLAS

@deprecated("Use edu.osu.compgeom.dataset.preprocess.NearestNeighborSearcher instead.", "0.1")
class KnnDatabase(val knnDatabaseDir: File, val dataset: CachedDataset, val shiftAmt: Int = 9) {
  def encodeDataset() = {
    if (!knnDatabaseDir.exists())
      knnDatabaseDir.mkdirs()
    dataset.origPoints.indices.par.foreach { i =>
      Timing((i + 1) + " of " + dataset.origPoints.size) {
        val a = getPermutationIndices(i)
        val f = new Array[Int](a.length)
        val lra = new LeftRightArray(dataset.origPoints.size)
        Permutation.perm2ffact(a, f, lra)
        val bs = packFactShifted(f, shiftAmt)
        withObjectOutputStream(new File(knnDatabaseDir, i + ".dat")) { oos =>
          bs.getWords.reverse.foreach(word => oos.writeLong(word))
        }
      }
    }
  }

  @inline
  final def getPermutationIndices(i: Int): Array[Int] = {
    class DistPair(val idx: Int, val dist: Double)
    val pairs = dataset.origPoints.indices.map { j =>
      val distSq = euclidDistSq(dataset.origPoints(i), dataset.origPoints(j))
      new DistPair(j, distSq)
    }.toArray
    java.util.Arrays.sort(pairs, new java.util.Comparator[DistPair] {
      def compare(a: DistPair, b: DistPair) = if (a.dist < b.dist) -1 else 1 //java.lang.Double.compare(a.dist, b.dist)
    })

    //    scala.util.Sorting.quickSort(pairs)
    pairs.iterator.map(_.idx).toArray
    //    scala.util.Sorting.stableSort(pairs).iterator.map(_.idx).toArray
  }

  final def getPermutationIndicesRange(i: Int, dots: Array[Double]): Array[Int] = {
    val n = dataset.origPoints(i).length
    @inline def dpDistSq(i: Int, j: Int): Double = {
      var dot = 0d
      var k = 0
      while (k < n) {
        dot += dataset.origPoints(i)(k) * dataset.origPoints(j)(k)
        k += 1
      }
      dots(i) + dots(j) - 2 * dot
    }

    class DistPair(val idx: Int, val dist: Double)

    val pairs = (0 until dataset.origPoints.size).map { j =>
      val distSq = dpDistSq(i, j)
      new DistPair(j, distSq)
    }.toArray
    java.util.Arrays.sort(pairs, new java.util.Comparator[DistPair] {
      def compare(a: DistPair, b: DistPair) = java.lang.Double.compare(a.dist, b.dist)
    })

    pairs.iterator.map(_.idx).toArray
  }

  @inline
  final def euclidDistSq(p1: Array[Float], p2: Array[Float]): Double = {
    var sum = 0d
    var i = 0
    val n = p1.length
    while (i < n) {
      val diff = p1(i) - p2(i)
      sum += diff * diff
      i += 1
    }
    sum
  }

  class TransparentBitSet extends BitSet(totalBitsNeeded) {
    def getWords = elems
  }

  def log2(x: Int) = math.ceil(math.log(x) / math.log(2)).toInt
  def packFactShifted(f: Array[Int], shiftAmt: Int): TransparentBitSet = {
    //    val totalBitsNeeded = f.indices.map { pos =>
    //      val numChoices = f.length - pos
    //      math.max(0, math.ceil(log2(numChoices)).toInt - shiftAmt)
    //    }.sum
    val bs = new TransparentBitSet

    var bitID = totalBitsNeeded - 1
    f.iterator.zipWithIndex.foreach {
      case (v, pos) =>
        val numChoices = f.length - pos
        val numBits = math.ceil(log2(numChoices)).toInt
        (numBits - 1 to shiftAmt by -1).foreach { ofs =>
          val mask = (1 << ofs)
          if ((v & mask) != 0) {
            bs += bitID
          }
          bitID -= 1
        }
    }
    bs
  }

  val totalBitsNeeded = dataset.origPoints.indices.map { pos =>
    val numChoices = dataset.origPoints.length - pos
    math.max(0, math.ceil(log2(numChoices)).toInt - shiftAmt)
  }.sum

  class UnpackFactShiftedIterator(bsIt: SavedBitSetIterator, length: Int, shiftAmt: Int) extends Iterator[Int] with Closeable {
    var i = 0
    def hasNext = i < length

    def next = {
      var ret = 0
      val numChoices = length - i
      val numBits = math.ceil(log2(numChoices)).toInt

      var bitID = numBits - 1
      while (bitID >= shiftAmt) {
        if (bsIt.next) {
          ret += 1 << bitID
        }
        bitID -= 1
      }
      i += 1
      ret
    }

    def close() = bsIt.close()
  }

  class SavedBitSetIterator(f: File) extends Iterator[Boolean] with Closeable {
    val ois = new ObjectInputStream(new BufferedInputStream(new FileInputStream(f)))
    var globalBitPos = totalBitsNeeded - 1
    var curWordID = globalBitPos >> 6
    var curWord = ois.readLong

    def hasNext = globalBitPos >= 0

    def next: Boolean = {
      val test = 1L << globalBitPos
      val ret = (curWord & test) != 0L

      globalBitPos -= 1
      val nextWordID = globalBitPos >> 6
      if (nextWordID != curWordID && globalBitPos >= 0) {
        curWordID = nextWordID
        curWord = ois.readLong
      }
      ret
    }

    override def finalize() = {
      close()
    }

    def close() = {
      ois.close
    }
  }

  /**
   * WARNING:  You *must* close() the returned iterator when you are done with it!
   */
  private[this] def getNearestNeighborUnpacker(i: Int): Iterator[Int] with Closeable = {
    val bsIt = new SavedBitSetIterator(new File(knnDatabaseDir, i + ".dat"))
    val gIt = new UnpackFactShiftedIterator(bsIt, dataset.origPoints.size, shiftAmt)
    val it = new FFact2PermShiftedIterator(gIt, i)
    it
  }

  def withNearestNeighborUnpacker[T](i: Int)(op: Iterator[Int] => T): T = {
    val unpacker = getNearestNeighborUnpacker(i)
    val result = op(unpacker)
    unpacker.close
    result
  }

  val rawLR = {
    val LR = new LeftRightArray(dataset.origPoints.size)
    LR.free_all()
    LR
  }

  @inline
  final def getFreshLR() = {
    val LR = new LeftRightArray(dataset.origPoints.size)
    LR.f_ = rawLR.f_
    LR.n_ = rawLR.n_
    System.arraycopy(rawLR.fl_, 0, LR.fl_, 0, rawLR.fl_.length)
    System.arraycopy(rawLR.tg_, 0, LR.tg_, 0, rawLR.tg_.length)
    LR
  }

  class FFact2PermShiftedIterator(fc: UnpackFactShiftedIterator, source: Int) extends Iterator[Int] with Closeable {
    val LR = getFreshLR()
    //    val LR = new LeftRightArray(dataset.neighbors.size)
    //    LR.free_all()
    val pcd = dataset.origPoints
    // Cache computed distances
    //val cachedDistances = Array.fill(pcd.length)(Double.MinValue)
    val cachedDistances = new Array[Double](pcd.length)
    java.util.Arrays.fill(cachedDistances, Double.MinValue)
    val used = new Array[Boolean](pcd.length)

    var k = 0
    def hasNext = (k < dataset.origPoints.length)
    def next = {
      var fcNearest = -1
      var nearestDistSq = Double.MaxValue
      val fcBase = fc.next
      (0 until (1 << shiftAmt)).foreach { add =>
        val fck = fcBase + add
        val i = LR.get_free_idx(fck)
        if (i != Integer.MIN_VALUE && !used(i)) {
          val dSq = if (cachedDistances(i) == Double.MinValue) {
            val dSq = euclidDistSq(pcd(source), pcd(i))
            cachedDistances(i) = dSq
            dSq
          } else {
            cachedDistances(i)
          }
          if (dSq < nearestDistSq) {
            nearestDistSq = dSq
            fcNearest = fck
          }
        }
      }
      val x_k = LR.get_free_idx_chg(fcNearest);
      used(x_k) = true
      k += 1
      x_k
    }

    def close() = fc.close()
  }
}

object CalcKnnInfo {
  def getPermIndicesBlas(subset: Range, pts: Array[Array[Float]], dots: Array[Float]): Array[Array[Int]] = {
    class DistPair(val idx: Int, val dist: Double)
    val distPairs = Array.ofDim[DistPair](subset.size, pts.size)

    val d = pts(0).length
    val S = new DenseMatrix(subset.size, d)
    subset.zipWithIndex.foreach {
      case (s, i) =>
        pts(s).indices.foreach { j =>
          S.set(i, j, pts(s)(j))
        }
    }
    
    val groupSize = 100

    pts.indices.grouped(groupSize).toSeq.par.foreach { group =>
      val X = new DenseMatrix(group.size, d)
      group.zipWithIndex.foreach {
        case (s, i) =>
          pts(s).indices.foreach { j =>
            X.set(i, j, pts(s)(j))
          }
      }

      val K = new DenseMatrix(subset.size, group.size)
      subset.zipWithIndex.foreach {
        case (i, row) =>
          group.zipWithIndex.foreach {
            case (j, col) =>
              K.set(row, col, dots(i) + dots(j))
          }
      }
      S.transBmultAdd(-2, X, K)

      subset.zipWithIndex.foreach {
        case (i, row) =>
          group.zipWithIndex.foreach {
            case (j, col) =>
              val blasResult = K.get(row, col)
              distPairs(row)(j) = new DistPair(j, blasResult)
          }
      }
    }

    val ret = distPairs.zipWithIndex.par.map{case (pairs, i) =>
      java.util.Arrays.sort(pairs, new java.util.Comparator[DistPair] {
        def compare(a: DistPair, b: DistPair) = if (a.dist < b.dist) -1 else 1 //java.lang.Double.compare(a.dist, b.dist)
      })
      pairs.map(_.idx)
    }.toArray
    
    return ret
  }
  
//  def getPermIndicesCuda(subset: Range, pts: Array[Array[Float]], dots: Array[Float]): Array[Array[Int]] = {
//
//    import no.uib.cipr.matrix._
//    class MyMat(numRows: Int, numColumns: Int) extends AbstractMatrix(numRows, numColumns) {
//      val data = Array.ofDim[Float](numRows * numColumns)
//      val ptr = new Pointer()
//      JCublas.cublasAlloc(numRows * numColumns, Sizeof.FLOAT, ptr)
//
//      def cublasSet(): Unit = {
//        JCublas.cublasSetVector(numRows * numColumns, Sizeof.FLOAT, Pointer.to(data), 1, ptr, 1);
//      }
//
//      def cublasFree(): Unit = {
//        JCublas.cublasFree(ptr)
//      }
//
//      @inline def set(row: Int, column: Int, v: Float): Unit = {
//        data(getIndex(row, column)) = v;
//      }
//
//      override def get(row: Int, column: Int): Double = {
//        return data(getIndex(row, column)).toDouble;
//      }
//
//      @inline def getIndex(row: Int, column: Int): Int = {
//        //check(row, column);
//        return row + column * numRows;
//      }
//
//      def checkTransBmultAdd(B: MyMat, C: MyMat): Unit = {
//        if (numColumns != B.numColumns)
//          throw new IndexOutOfBoundsException(
//            "A.numColumns != B.numColumns (" + numColumns + " != "
//              + B.numColumns + ")");
//        if (numRows != C.numRows)
//          throw new IndexOutOfBoundsException("A.numRows != C.numRows ("
//            + numRows + " != " + C.numRows + ")");
//        if (B.numRows != C.numColumns)
//          throw new IndexOutOfBoundsException("B.numRows != C.numColumns ("
//            + B.numRows + " != " + C.numColumns + ")");
//      }
//
//      def transBmultAdd(alpha: Float, B: MyMat, C: MyMat): MyMat = {
//        checkTransBmultAdd(B, C);
//
//        val Bd = B.data
//        val Cd = C.data
//
//        JCublas.cublasSgemm(
//          'n', 't', C.numRows, C.numColumns, numColumns, alpha, ptr, numRows, B.ptr, B.numRows, 1, C.ptr, C.numRows);
//
//        // Copy the result from the device to the host
//        JCublas.cublasGetVector(C.data.length, Sizeof.FLOAT, C.ptr, 1, Pointer.to(Cd), 1);
//        return C
//      }
//    }
//
//    class DistPair(val idx: Int, val dist: Double)
//    val distPairs = Array.ofDim[DistPair](subset.size, pts.size)
//
//    JCublas.cublasInit()
//
//    val d = pts(0).length
//    val S = new MyMat(subset.size, d)
//    subset.zipWithIndex.foreach {
//      case (s, i) =>
//        pts(s).indices.foreach { j =>
//          S.set(i, j, pts(s)(j))
//        }
//    }
//    S.cublasSet()
//    
//    val groupSize = 500
//
//    pts.indices.grouped(groupSize).foreach { group =>
//      val X = new MyMat(group.size, d)
//      group.zipWithIndex.foreach {
//        case (s, i) =>
//          pts(s).indices.foreach { j =>
//            X.set(i, j, pts(s)(j))
//          }
//      }
//      X.cublasSet()
//
//      val K = new MyMat(subset.size, group.size)
//      subset.zipWithIndex.foreach {
//        case (i, row) =>
//          group.zipWithIndex.foreach {
//            case (j, col) =>
//              K.set(row, col, dots(i) + dots(j))
//          }
//      }
//      K.cublasSet()
//      //      S.transBmultAdd(1, X, K)
//      S.transBmultAdd(-2, X, K)
//
//      X.cublasFree()
//      K.cublasFree()
//
//      subset.zipWithIndex.foreach {
//        case (i, row) =>
//          group.zipWithIndex.foreach {
//            case (j, col) =>
//              val blasResult = K.get(row, col)
//              distPairs(row)(j) = new DistPair(j, blasResult)
//          }
//      }
//    }
//    S.cublasFree()
//    JCublas.cublasShutdown()
//
//    val ret = distPairs.zipWithIndex.map{case (pairs, i) =>
//      java.util.Arrays.sort(pairs, new java.util.Comparator[DistPair] {
//        def compare(a: DistPair, b: DistPair) = if (a.dist < b.dist) -1 else 1 //java.lang.Double.compare(a.dist, b.dist)
//      })
//      pairs.map(_.idx)
//    }
//    
//    return ret
//  }

  /*
  def main(args: Array[String]): Unit = {
    
    if (args.size != 2) {
      println("Arguments:  dataset_dir neighbor_info_output_dir")
      System.exit(0)
    }

    val dataset = new CachedDataset(new File(args(0))) with FilePdbStreamProvider
    
    val nns = new NearestNeighborSearcher(dataset.origPoints)
    val knnDatabase = new KnnDatabase(new File(args(1)), dataset, 9)
    (0 until 252996).foreach{x =>
      val p1 = Timing("fast"){nns.getNearestNeighbors(x, 21)}
      val p2 = Timing("brute"){knnDatabase.getPermutationIndices(x).take(21)}
      println("Found:  " + p1.mkString(","))
      println("GT:     " + p2.mkString(","))
    }
//    val s = 1
//    val p1 = nns.getNearestNeighbors(s, 15)
//    println("Found:  " + p1.mkString(","))
//    
//    val knnDatabase = new KnnDatabase(new File(args(1)), dataset, 9)
//    val p2 = knnDatabase.getPermutationIndices(s).take(15)
//    println("GT:     " + p2.mkString(","))
    
//    p1.map(y => knnDatabase.euclidDistSq(dataset.origPoints(s), dataset.origPoints(y))).foreach(d => println("\t" + d))
//    println()
//    
//    p2.map(y => knnDatabase.euclidDistSq(dataset.origPoints(s), dataset.origPoints(y))).foreach(d => println("\t" + d))
//    
    
    
    
//    val backbone = dataset.getCarbonBackbone(0)
//    println("Size:  " + backbone.length)
    
    System.exit(0)
    
    //    knnDatabase.encodeDataset()
    
    val dots = Timing("Self dot-products") {
      dataset.origPoints.par.map { p => p.iterator.map(x => x * x).sum }.toArray
    }
    
    val n = 200

    val cudaPerms = Timing("BLAS"){getPermIndicesBlas(dataset.origPoints.indices.take(n), dataset.origPoints, dots)}.map(_.take(30))

    val firstTen = Timing("Standard"){dataset.origPoints.indices.take(n).par.map { i =>
      knnDatabase.getPermutationIndices(i).take(30)
    }.toArray}
    
//    cudaPerms.zip(firstTen).foreach(pair => println(pair._1.zip(pair._2).mkString(", ")))
    
    
    //
    //    dataset.origPoints.indices.grouped(10)

    println("Done")

    //    dataset.origPoints.indices.foreach{i =>
    //      val p = knnDatabase.getPermutationIndices(i).take(30)
    //      val q = knnDatabase.withNearestNeighborUnpacker(i){_.take(30).toArray}
    //      val r = dataset.neighbors(i).take(30)
    //      println(p.mkString(","))
    //      println(q.mkString(","))
    //      println(r.mkString(","))
    //      println
    //      println(p.map(y => knnDatabase.euclidDistSq(dataset.origPoints(i), dataset.origPoints(y))).mkString(","))
    //      println(r.map(y => knnDatabase.euclidDistSq(dataset.origPoints(i), dataset.origPoints(y))).mkString(","))
    //      Thread.sleep(5000)
    //    }

    System.exit(0)

    //    val outDir = new File("/media/My Passport/trpcage_neighbors_v2")
    //    
    //    knnDatabase.knnDatabaseDir.listFiles.par.foreach{f =>
    //      val i = f.getName.takeWhile(_ != '.').toInt
    //      println((i+1) + " of " + dataset.origPoints.length)
    //      val bs = withObjectInputStream(f){_.readObject.asInstanceOf[BitSet]}
    //      val tbs = new knnDatabase.TransparentBitSet
    //      bs.foreach(x => tbs += x)
    //      
    //      withObjectOutputStream(new File(outDir, i + ".dat")) { oos =>
    //        tbs.getWords.reverse.foreach(word => oos.writeLong(word))
    //      }
    //    }


//    dataset.origPoints.indices.foreach { i =>
//      //      val q = knnDatabase.withNearestNeighborUnpacker(i){_.take(30).toArray}
//      val r = Timing("a") { knnDatabase.getPermutationIndices(i) }
//      println(r.take(30).mkString(","))
//      Thread.sleep(1000)
//    }

    //      Timing("Main method") {
    //        dataset.origPoints.indices.foreach { i =>
    //          //Timing("idx") {println(knnDatabase.getNearestNeighbors(i).take(30).mkString(","))}
    //          val shiftAmt = 9
    //          val a = knnDatabase.getPermutationIndices(i)
    //	        val f = new Array[Int](a.length)
    //	        val lra = new LeftRightArray(dataset.origPoints.size)
    //	        Permutation.perm2ffact(a, f, lra)
    //	        val bs = knnDatabase.packFactShifted(f, shiftAmt)
    //	        withObjectOutputStream(new File("/home/harveywi/Desktop/temp", i + ".dat")) { oos =>
    //	          bs.getWords.reverse.foreach(word => oos.writeLong(word))
    //	        }
    //          
    //          val nn = knnDatabase.getNearestNeighborUnpacker(i)
    //          nn.close()
    //        }
    //      }

  }
  */
}