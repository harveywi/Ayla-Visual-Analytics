package edu.osu.compgeom.dataset.preprocess

import java.io._
import scopt.OptionParser
import javax.vecmath.Point3d
import edu.osu.compgeom.util.IO._
import edu.osu.compgeom.pdb.PDB

object PreprocessConformationsApproximateKnn {

  class CommandLineConfig {
    var datasetDir: File = null
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
    }

    if (!parser.parse(args))
      System.exit(0)

    val conformationFilenamesFile = new File(args(0), "conformation_filenames.txt")
    require(conformationFilenamesFile.exists, "Error:  The file called 'conformation_filenames.txt' was not found in %s.".format(config.datasetDir.getAbsolutePath()))

    val nAlphaCarbonAtoms = withBufferedReader(conformationFilenamesFile){br => 
      val backbone = PDB.getAlphaCarbonBackbone(new File(br.readLine))
      backbone.size
    }
    println("Backbone size:  " + nAlphaCarbonAtoms)
    val backboneOut = new LargeFloatVector(new File(config.datasetDir, "backbones.dat"), 3*nAlphaCarbonAtoms*252996)
    
    val nPairs = (nAlphaCarbonAtoms*(nAlphaCarbonAtoms-1)) / 2
    val pairwiseDistsOut = new LargeDoubleVector(new File("/media/intel_ssd/example.dat"), nPairs*252996)

    withBufferedReader(conformationFilenamesFile) { br =>
      Iterator.continually(br.readLine).takeWhile(_ != null).map(new File(_)).zipWithIndex.foreach{case (pdbFile, pdbIdx) => {
        println("Processing " + (pdbIdx+1) + " of 252996")
        val alphaCarbonBackbone = PDB.getAlphaCarbonBackbone(pdbFile)
        
        alphaCarbonBackbone.indices.foreach{i =>
          val ofs = 3*nAlphaCarbonAtoms*pdbIdx + 3*i
          val p = alphaCarbonBackbone(i)
          backboneOut.set(ofs, p.x)
          backboneOut.set(ofs+1, p.y)
          backboneOut.set(ofs+2, p.z)
        }

        val n = alphaCarbonBackbone.size

        val pairwiseDists = (0 until n).flatMap(i => {
          (i + 1 until n).map(j => {
            alphaCarbonBackbone(i).distance(alphaCarbonBackbone(j)).toDouble
          })
        })
        println(pairwiseDists.size)

        pairwiseDists.indices.foreach{i =>
          val ofs = nPairs*pdbIdx + i
          pairwiseDistsOut.set(ofs, pairwiseDists(i))
        }
      }}
    }

    backboneOut.close
    pairwiseDistsOut.close
  }

    import java.io.RandomAccessFile;
  import java.nio.MappedByteBuffer;
  import java.nio.channels.FileChannel;
  import java.util.ArrayList;
  import java.util.List;
  import sun.misc.Cleaner
  import sun.nio.ch.DirectBuffer
  
  abstract class LargeMemoryMappedVector[@specialized T](val file: File, val n: Long) extends Closeable {
    private[this] val raf = new RandomAccessFile(file, "rw")
    def size: Long
    private[this] val MAPPING_SIZE = 1 << 30
    private[this] val mappings: java.util.List[MappedByteBuffer] = new java.util.ArrayList()
    
    def getFromMBB(mbb: MappedByteBuffer, i: Int): T
    def putToMBB(mbb: MappedByteBuffer, i: Int, t: T): Unit

    Range.Long(0, size, MAPPING_SIZE).foreach(offset => {
      println("Offset:  " + offset)
      val size2 = math.min(size - offset, MAPPING_SIZE)
      val byteBuff = raf.getChannel().map(FileChannel.MapMode.READ_WRITE, offset, size2)
      mappings.add(byteBuff)
    })
    
    def get(i: Int): T = {
      assert(i >= 0 && i < n)
      val p = i * 8
      val mapN = (p / MAPPING_SIZE).toInt;
      val offN = (p % MAPPING_SIZE).toInt;
      getFromMBB(mappings.get(mapN), offN)
    }

    def set(i: Int, t: T): Unit = {
      assert(i >= 0 && i < n);
      val p = i * 8;
      val mapN = (p / MAPPING_SIZE).toInt;
      val offN = (p % MAPPING_SIZE).toInt;
      putToMBB(mappings.get(mapN), offN, t)
    }

    def close(): Unit = {
      val it = mappings.iterator()
      while (it.hasNext()) {
        val mapping = it.next()
        clean(mapping)
      }
      raf.close()
    }

    private def clean(mapping: MappedByteBuffer): Unit = {
      if (mapping == null) return ;
      val cleaner = mapping.asInstanceOf[DirectBuffer].cleaner()
      if (cleaner != null) cleaner.clean();
    }    
  }
  
  class LargeDoubleVector(file: File, n: Long) extends LargeMemoryMappedVector[Double](file, n) {
    def getFromMBB(mbb: MappedByteBuffer, i: Int): Double = mbb.getDouble(i)
    def putToMBB(mbb: MappedByteBuffer, i: Int, d: Double): Unit = mbb.putDouble(i, d)
    override def size = 8L * n
  }
  
  class LargeFloatVector(file: File, n: Long) extends LargeMemoryMappedVector[Float](file, n) {
    def getFromMBB(mbb: MappedByteBuffer, i: Int): Float = mbb.getFloat(i)
    def putToMBB(mbb: MappedByteBuffer, i: Int, f: Float): Unit = mbb.putFloat(i, f)
    override def size = 4L * n
  }

//  class LargeDoubleVector(val filename: String, n: Long) extends Closeable {
//    val raf = new RandomAccessFile(filename, "rw")
//    val size = 8L * n
//    val MAPPING_SIZE = 1 << 30
//    val mappings: java.util.List[MappedByteBuffer] = new java.util.ArrayList()
//
//    Range.Long(0, size, MAPPING_SIZE).foreach { offset =>
//      val size2 = math.min(size - offset, MAPPING_SIZE)
//      val byteBuff = raf.getChannel().map(FileChannel.MapMode.READ_WRITE, offset, size2)
//      mappings.add(byteBuff)
//    }
//
//    def get(i: Int): Double = {
//      assert(i >= 0 && i < n)
//      val p = i * 8
//      val mapN = (p / MAPPING_SIZE).toInt;
//      val offN = (p % MAPPING_SIZE).toInt;
//      mappings.get(mapN).getDouble(offN);
//    }
//
//    def set(i: Int, d: Double): Unit = {
//      assert(i >= 0 && i < n);
//      val p = i * 8;
//      val mapN = (p / MAPPING_SIZE).toInt;
//      val offN = (p % MAPPING_SIZE).toInt;
//      mappings.get(mapN).putDouble(offN, d);
//    }
//
//    def close(): Unit = {
//      val it = mappings.iterator()
//      while (it.hasNext()) {
//        val mapping = it.next()
//        clean(mapping)
//      }
//      raf.close()
//    }
//
//    private def clean(mapping: MappedByteBuffer): Unit = {
//      if (mapping == null) return ;
//      val cleaner = mapping.asInstanceOf[DirectBuffer].cleaner()
//      if (cleaner != null) cleaner.clean();
//    }
//  }
}