/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.dataset

import java.io._
import org.apache.commons.compress.archivers.zip._
import org.jgrapht.graph.SimpleDirectedGraph
import org.jgrapht.graph.DefaultEdge
import scala.collection.mutable.ArrayBuffer
import ayla.util.Timing
import ayla.util.IO._
import ayla.linalg.PointCloudData
import scalaz.Validation._
import scalaz._
import Scalaz._
import ayla.pdb.PdbStreamProvider
import ayla.pdb.PdbStreamProvider

class CachedDataset(
    val origPoints: Array[Array[Float]],
    val scalarArrayFiles: Array[File], 
    val dsspOutput: Option[Array[Char]],
    val pdbStreamProvider: PdbStreamProvider) {
//  var origPoints: Array[Array[Float]] = null
//  def loadOrigPoints(): Unit = {
//    origPoints = Timing("Loading original points") {
//      PointCloudData.load(new File(dir, "pca3D_v3.dat"))
//    }
//  }

  //  val pcaPoints: Array[Array[Float]] = {
  //    Timing("Loading PCA points") {
  //      PointCloudData.load(new File(dir, "pca3D_v3.dat"))
  ////      withObjectInputStream(new File(dir, "pca3D_v3.dat"))(_.readObject().asInstanceOf[Array[Array[Float]]])
  //    }
  //  }

//  val colorFunctions = {
//    val stack = new scala.collection.mutable.Stack[File]
//    val scalarFunctionDir = new File(dir, "scalar_functions")
//    stack.push(scalarFunctionDir)
//
//    val ret = new ArrayBuffer[(File, String)]()
//
//    while (!stack.isEmpty) {
//      val f = stack.pop
//      if (f.isDirectory) {
//        val childFiles = f.listFiles.filter(child => child.getName().endsWith(".txt") || child.isDirectory)
//        childFiles.foreach { childFile =>
//          if (childFile.isDirectory) {
//            stack.push(childFile)
//          } else {
//            val funcName = withBufferedReader(childFile) { _.readLine() }
//            ret += ((childFile, funcName))
//          }
//        }
//      }
//    }
//    ret.toArray
//  }

  def getScalarArray(file: File): Array[Float] = scala.io.Source.fromFile(file).getLines.map(_.toFloat).toArray

//  val dsspOutput: Option[Array[Char]] = {
//    val dsspFile = new File(dir, "secondary_structure_dssp.dat")
//    if (!dsspFile.exists()) {
//      None
//    } else {
//      val ois = new ObjectInputStream(new FileInputStream(dsspFile))
//      Some(ois.readObject.asInstanceOf[Array[Char]])
//    }
//  }
}

object CachedDataset {
  import ayla.util.tools._

  private[this] def getTextFilesRecursively(f: File): Array[File] = {
    val (dirs, nonDirs) = f.listFiles.partition(_.isDirectory)
    nonDirs.filter(_.getName.endsWith(".txt")) ++ dirs.flatMap(getTextFilesRecursively)
  }
  
  def apply(datasetDir: File): Validation[String, CachedDataset] = {
    implicit val dDir = datasetDir
    
    def getPcd = fileOpt("pcd_v3.dat") match {
      case Some(f) if !f.isDirectory => catchException(PointCloudData.load(f))
      case None => Failure("File not found:  pcd_v3.dat")
    }
    
    def getDsspOutput = fileOpt("secondary_structure_dssp.dat") match {
      case Some(f) => catchException(Some(withObjectInputStream(f)(_.readObject.asInstanceOf[Array[Char]])))
      case None => Success(none[Array[Char]])
    }

    def getScalarFunctionFiles = new File(datasetDir, "scalar_functions") match {
      case sfDir if sfDir.exists && sfDir.isDirectory => catchException(getTextFilesRecursively(sfDir))
      case f @ _ => Failure(s"Directory not found: ${f.getAbsolutePath}")
    }
    
    for {
      pcd <- getPcd
      scalarFunctionFiles <- getScalarFunctionFiles
      dsspOutput <- getDsspOutput
      pdbStreamProvider <- PdbStreamProvider(datasetDir)
    } yield new CachedDataset(pcd, scalarFunctionFiles, dsspOutput, pdbStreamProvider)
  }
}
