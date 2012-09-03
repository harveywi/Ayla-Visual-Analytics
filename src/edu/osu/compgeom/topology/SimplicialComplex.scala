package edu.osu.compgeom.topology

import scala.collection.mutable.ArrayBuffer
import java.awt.Color
import java.io._
import java.util.Comparator
import scala.collection.JavaConversions._
import org.jgrapht.graph.Multigraph
import org.jgrapht.graph.DefaultDirectedGraph
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.DefaultWeightedEdge
import edu.osu.compgeom.util.UnionFind

@SerialVersionUID(1L)
class SimplicialComplex(val vertices: Array[Array[Float]], val faces: Array[Array[Int]]) extends Serializable {
	val adjList: Array[Array[Int]] = {
	  val neighborCounts = new Array[Int](vertices.size)
	  faces.foreach(f => {
	    (0 until f.length).foreach{i =>
	      (i+1 until f.length).foreach{j =>
	        neighborCounts(f(i)) += 1
	        neighborCounts(f(j)) += 1
	      }
	    }
	  })
	  
	  val neighborLists = neighborCounts.map(new Array[Int](_))
	  val nextIdx = new Array[Int](vertices.size)
	  faces.foreach(f => {
	    (0 until f.length).foreach{i =>
	      (i+1 until f.length).foreach{j =>
	        neighborLists(f(i))(nextIdx(f(i))) = f(j)
		      nextIdx(f(i)) += 1
		      
		      neighborLists(f(j))(nextIdx(f(j))) = f(i)
		      nextIdx(f(j)) += 1
	      }
	    }
	  })	  
	  neighborLists
	}

  def getNeighbors(v: Int): Array[Int] = adjList(v)

  /**
   * Returns the number of connected components
   */
  lazy val bettiZero: Int = {
    val uf = new UnionFind(vertices.size)
    adjList.indices.foreach(i => adjList(i).foreach(j => {
      uf.union(i, j)
    }))
    adjList.indices.map(uf.find).toSet.size
  }

  class SCPersistencePair(val extremum: Int, val saddle: Int, val killedBy: Int, val persistence: Float) extends Ordered[SCPersistencePair] {
    override def compare(o: SCPersistencePair) = persistence.compare(o.persistence)
  }

  def toObjFile(objFile: File): Unit = {
    val bw = new BufferedWriter(new FileWriter(objFile))
    vertices.foreach(v => bw.write(v.mkString("v ", " ", "\n")))
    // Remember - obj file vertex indices are base 1.
    for (f <- faces.map(_.map(_ + 1))) {
      f.length match {
        case 2 => { bw.write(f.mkString("e ", " ", "\n")) }
        case _ => { bw.write(f.mkString("f ", " ", "\n")) }
      }
    }
    bw.flush
    bw.close
  }

  def toOffFile(offFile: File): Unit = {

    val faceEdges =
      (for (
        face <- faces if face.length > 2;
        i <- 0 until face.length;
        j <- i + 1 until face.length
      ) yield {
        (math.min(i, j), math.max(i, j))
      }).toSet

    val edgesToWrite = faces.filter(_.length == 2).filter(e => !faceEdges.contains((math.min(e(0), e(1)), math.max(e(0), e(1)))))

    val bw = new BufferedWriter(new FileWriter(offFile))
    bw.write("OFF\n")
    bw.write(List(vertices.size, faces.size, edgesToWrite.size).mkString("", " ", "\n"))
    for (v <- vertices)
      bw.write(v.mkString("", " ", "\n"))

    for (e <- edgesToWrite)
      bw.write(e.mkString(e.length + " ", " ", "\n"))

    for (f <- faces.filter(_.size > 2))
      bw.write(f.mkString(f.length + " ", " ", "\n"))

    bw.write("\n")
    bw.flush
    bw.close
  }
}

object SimplicialComplex {

  def fromObjFile(objFile: File): SimplicialComplex = {
    // Assumption:  Vertex definitions followed by face/edge definitions. No other kinds of stuff allowed (e.g.
    // texture coordinates are verboten).  Also, the vertices are referenced *base 1* in the file.
    val br = new BufferedReader(new FileReader(objFile))
    val vertices = new ArrayBuffer[Array[Float]]
    val faces = new ArrayBuffer[Array[Int]]

    while (br.ready) {
      val line = br.readLine.trim
      val firstChar = line(0).toLower

      firstChar match {
        case '#' => {
          println("Ignoring comment: " + line)
        }
        case 'v' => {
          val v = (line.split("\\s+").drop(1).map(_.toFloat)).toArray
          vertices += v
        }
        case 'e' => {
          val e = (line.split("\\s+").drop(1).map(_.toInt - 1)).toArray
          faces += e
        }
        case 'f' => {
          val f = (line.split("\\s+").drop(1).map(_.toInt - 1)).toArray
          faces += f
        }
        case _ => throw new IOException("Cannot parse line:" + line)
      }
    }

    br.close
    return new SimplicialComplex(vertices.toArray, faces.toArray)
  }

  def fromOffFile(offFile: File): SimplicialComplex = {
    // Keep track of OFF file parsing state
    object Mode extends Enumeration {
      type Mode = Value
      val OFF = Value
      val VFE = Value
      val VERTS = Value
      val FACES = Value
    }

    var mode = Mode.OFF
    val br = new BufferedReader(new FileReader(offFile))
    var nVerts = -1
    var nFaces = -1

    val vertices = new ArrayBuffer[Array[Float]]
    val faces = new ArrayBuffer[Array[Int]]

    while (br.ready()) {
      val line = br.readLine.trim
      if (line.length != 0 && !line.startsWith("#")) {
        val lineSplit = line.split("\\s+")
        mode match {
          case Mode.OFF => { mode = Mode.VFE }
          case Mode.VFE => {
            nVerts = lineSplit(0).toInt
            nFaces = lineSplit(1).toInt
            mode = Mode.VERTS
          }
          case Mode.VERTS => {
            val vert = (lineSplit.map(_.toFloat)).toArray
            vertices += vert
            if (vertices.size == nVerts)
              mode = Mode.FACES
          }
          case Mode.FACES => {
            val faceSize = lineSplit(0).toInt
            val face = lineSplit.drop(1).take(faceSize).map(_.toInt).toArray //(lineSplit.map(_.toInt).drop(1)).toArray
            faces += face
            if (faces.size == nFaces)
              println("Done reading faces.")
          }
          case _ => throw new IllegalStateException
        }
      }
    }

    br.close
    return new SimplicialComplex(vertices.toArray, faces.toArray)
  }
}
