package edu.osu.compgeom.dataset.preprocess

import edu.osu.compgeom.util.IO._
import java.io._
import edu.osu.compgeom.dataset.ZipFilePdbStreamProvider
import edu.osu.compgeom.dataset.FilePdbStreamProvider
import edu.osu.compgeom.dataset.PdbStreamProvider
import edu.osu.compgeom.linalg.PCA

object PreprocessConformations2 {

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
    val pcaSampleSize = math.min(pdbProvider.size, 3000)
    val sampleIndices = Range(0, pdbProvider.size).sortWith((_, _) => math.random < .5).take(pcaSampleSize)

    println("Sampling point cloud data for PCA")
    val pcaSample = sampleIndices.map(pdbProvider.getSurvivinF93_F101_PairwiseDistVec).toArray
    println("Performing (approximate) PCA")
    val pcaSampleMean = getMeanVec(pcaSample)
    pcaSample.foreach{s =>
      s.indices.foreach(i => s(i) -= pcaSampleMean(i))
    }
    val evecs = PCA.getPrincipalComponents(pcaSample, math.min(pcaSampleMean.length-1, 500), pcaSampleMean)
    
//    val pcdOut = new ObjectOutputStream(new FileOutputStream(new File(datasetDir, "pcdNew.dat")))
//    val pcd3dOut = new ObjectOutputStream(new FileOutputStream(new File(datasetDir, "pca3DNew.dat")))
    
    val pcd = new Array[Array[Float]](pdbProvider.size)
    
    println("Projecting conformations onto approximate PCA basis")
    (0 until pdbProvider.size).par.foreach { conformationID =>
      println((conformationID + 1) + " of " + pdbProvider.size)
      val p = pdbProvider.getSurvivinF93_F101_PairwiseDistVec(conformationID)
      pcd(conformationID) = evecs.par.map(evec => pcaProject(p, evec, pcaSampleMean)).seq.toArray
    }
    
    withObjectOutputStream(new File(datasetDir, "pcd_v2.dat")){oos =>
      oos.writeObject(pcd)
    }
    
    withObjectOutputStream(new File(datasetDir, "pca3D_v2.dat")){oos =>
      val out = pcd.map(p => p.take(3))
      oos.writeObject(out)
    }
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

//  @tailrec
//  def pcaProject(p: Array[Double], evec: Array[Double], pcaSampleMean: Array[Double], i: Int = 0, curSum: Double = 0): Float = {
//    if (i == p.size)
//      curSum.toFloat
//    else {
//      val curSumNew = curSum + (p(i) - pcaSampleMean(i)) * evec(i)
//      pcaProject(p, evec, pcaSampleMean, i + 1, curSumNew)
//    }
//  }

  def getMeanVec(pts: Array[Array[Double]]): Array[Double] = {
    val pMean = new Array[Double](pts(0).size)
    pts.foreach { p =>
      p.indices.foreach(j => pMean(j) += p(j))
    }
    pMean.indices.foreach(j => pMean(j) /= pts.size.toDouble)

    return pMean
  }

}