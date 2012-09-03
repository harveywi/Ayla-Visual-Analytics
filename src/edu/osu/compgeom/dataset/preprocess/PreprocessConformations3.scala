package edu.osu.compgeom.dataset.preprocess

import edu.osu.compgeom.util.IO._
import java.io._
import edu.osu.compgeom.dataset.ZipFilePdbStreamProvider
import edu.osu.compgeom.dataset.FilePdbStreamProvider
import edu.osu.compgeom.dataset.PdbStreamProvider
import edu.osu.compgeom.linalg.PCA

object PreprocessConformations3 {

  def main(args: Array[String]): Unit = {
    if (args.size != 2) {
      println("Usage:  dataset_dir pdb_provider")
      println("pdb_provider must be either 'zip' or 'file'")
      System.exit(0)
    }

    val datasetDir = new File(args(0))
    require(datasetDir.exists, "File not found:  %s".format(datasetDir.getAbsolutePath()))

    val pdbProvider = args(1) match {
      case "zip" => new { val dir = datasetDir } with ZipFilePdbStreamProvider
      case "file" => new { val dir = datasetDir } with FilePdbStreamProvider
      case _ => throw new IllegalArgumentException("pdb_provider must be either 'zip' or 'file'")
    }

    println("Number of conformations:  " + pdbProvider.size)

    dumpPointCloudData(pdbProvider, datasetDir)
  }

  def dumpPointCloudData(pdbProvider: PdbStreamProvider, datasetDir: File): Unit = {
    val pcd = new Array[Array[Float]](pdbProvider.size)
    
    println("Transforming conformations into put cloud data")
    (0 until pdbProvider.size).par.foreach { conformationID =>
      println((conformationID + 1) + " of " + pdbProvider.size)
      val p = pdbProvider.getBackboneVectorK5(conformationID).map(_.toFloat)
      pcd(conformationID) = p
    }
    
    withObjectOutputStream(new File(datasetDir, "pcd_v3.dat")){oos =>
      oos.writeObject(pcd)
    }
    
//    withObjectOutputStream(new File(datasetDir, "pca3D_v2.dat")){oos =>
//      val out = pcd.map(p => p.take(3))
//      oos.writeObject(out)
//    }
  }
}