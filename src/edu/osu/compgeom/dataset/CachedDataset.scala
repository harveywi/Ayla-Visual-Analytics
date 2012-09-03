package edu.osu.compgeom.dataset

import java.io._
import edu.osu.compgeom.util.IO._
import edu.osu.compgeom.util.Timing
import org.apache.commons.compress.archivers.zip._
import org.jgrapht.graph.SimpleDirectedGraph
import org.jgrapht.graph.DefaultEdge
import scala.collection.mutable.ArrayBuffer

/**
 * Mix this in with a PdbStreamProvider
 */
abstract class CachedDataset(val dir: File) extends Dataset {
  var origPoints: Array[Array[Float]] = null
  def loadOrigPoints(): Unit = {
    origPoints = Timing("Loading original points") {
      withObjectInputStream(new File(dir, "pcd_v2.dat"))(_.readObject().asInstanceOf[Array[Array[Float]]])
    }
  }
  
  val pcaPoints: Array[Array[Float]] = {
    Timing("Loading PCA points") {
      withObjectInputStream(new File(dir, "pca3D_v2.dat"))(_.readObject().asInstanceOf[Array[Array[Float]]])
    }
  }

  val colorFunctions = {
    val stack = new scala.collection.mutable.Stack[File]
    val scalarFunctionDir = new File(dir, "scalar_functions")
    stack.push(scalarFunctionDir)

    val ret = new ArrayBuffer[(File, String)]()

    while (!stack.isEmpty) {
      val f = stack.pop
      if (f.isDirectory) {
        val childFiles = f.listFiles.filter(child => child.getName().endsWith(".txt") || child.isDirectory)
        childFiles.foreach { childFile =>
          if (childFile.isDirectory) {
            stack.push(childFile)
          } else {
            val funcName = withBufferedReader(childFile) { _.readLine() }
            ret += ((childFile, funcName))
          }
        }
      }
    }
    ret.toArray
  }

  def getColorFunction(file: File): Array[Float] = {
    withBufferedReader(file) { br => Iterator.continually(br.readLine).takeWhile(_ != null).drop(1).map(_.toFloat).toArray }
  }

  val dsspOutput: Option[Array[Char]] = {
    val dsspFile = new File(dir, "secondary_structure_dssp.dat")
    if (!dsspFile.exists()) {
      None
    } else {
      val ois = new ObjectInputStream(new FileInputStream(dsspFile))
      Some(ois.readObject.asInstanceOf[Array[Char]])
    }
  }
}