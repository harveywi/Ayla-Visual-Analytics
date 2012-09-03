package edu.osu.compgeom.topology

import scala.sys.process._
import java.io._
import edu.osu.compgeom.topology._
import edu.osu.compgeom.util.Timing
import edu.osu.compgeom.util.IO._
import scala.collection.JavaConverters._
import scala.Console
import edu.osu.compgeom.util.UnionFind
import scala.annotation.tailrec

class ReebAlgorithm2 {

}

object ReebAlgorithm2 {
  val reebCmd1 = "/home/harveywi/bin/ReebGraph/ReebGraph %s -a %s"
  val reebCmd2 = "/home/harveywi/bin/ReebGraph2/ReebGraph %s %s"
  val dir = new File("/home/harveywi/research/off/")
  val pairs = List(
    ("earthquake.off", 3),
    ("plasma.off", 3),
    ("buckyball.off", 3),
    ("dragon.off", 1),
    ("neptune.off", 1),
    ("post.off", 3),
    ("chair_uniform.off", 1),
    ("protein324.0.off", 3),
    ("blunt.off", 3),
    ("kitten_uniform.off", 1),
    ("BostonTeapot.isov=50.off", 1),
    ("engine.isov=60.off", 1),
    ("fighter.off", 3),
    ("protein256.0.off", 3),
    ("protein196.0.off", 3),
    ("764_hand-olivier-10kf.off", 1),
    ("pyramid.off", 1))

  def main(args: Array[String]): Unit = {
    //    Console.setOut(new PrintStream(System.out) {
    //      override def println(s: String) = {
    //        if (s.startsWith("\t"))
    //        	super.print(s)
    //      }
    //    })
    //    triples
    //    check2
        while (true)
        	triples(20)
//    chipotle(70000)
  }

  def pruneFaces(faces: Array[Array[Int]]): Array[Array[Int]] = {
    class Face(val f: Array[Int], val id: Int) {
      val indices = List(0, 1, 2)
      def apply(i: Int) = f(i)

      var ufParent = this

//      def find: Face = {
//        if (this == ufParent) {
//          return ufParent
//        } else {
//          ufParent = ufParent.find
//          return ufParent
//        }
//      }
      
      def find: Face = {
        ufParent = findRec(this)
        ufParent
      }
      
      @tailrec
      private final def findRec(parent: Face): Face = {
        if (parent.ufParent == parent)
          parent
          else
        findRec(parent.ufParent)
      }

      def toArray = f

      override def toString = f.mkString("[", ",", "]")

      override def equals(any: Any): Boolean = any match {
        case o: Face => f(0) == o(0) && f(1) == o(1) && f(2) == o(2)
        case _ => false
      }
      override def hashCode = f.hashCode()
    }
    
    class Up(val face: Face) extends Comparable[Up] {
      val indices = List(0,1,2)
      override def compareTo(o: Up): Int = {
        indices.foreach { i =>
          val c = face(i).compare(o.face(i))
          if (c != 0) {
            return c
          }
        }
        return 0
      }
      override def toString = face.toString
    }
    
    class Down(val face: Face) extends Comparable[Down] {
      val indices = List(2, 1, 0)
      override def compareTo(o: Down): Int = {
        indices.foreach { i =>
          val c = face(i).compare(o.face(i))
          if (c != 0) {
            return c
          }
        }
        return 0
      }
      override def toString = face.toString
    }

    val blahFaces = faces.zipWithIndex.map{case (f, i) => new Face(f, i)}
    
    val upTree = new java.util.TreeSet[Up]
    blahFaces.foreach{face => upTree.add(new Up(face))}
    
    val downTree = new java.util.TreeSet[Down]
    blahFaces.foreach{face => downTree.add(new Down(face))}
    
    blahFaces.foreach {face =>
      val x = new Up(new Face(Array(face(0), face(2), Int.MaxValue), -1))
      val a = upTree.lower(x)
      if (a != -1 && a.face(0) == face(0) && a.face(1) == face(2)) {
      	face.ufParent = a.face
      } else {
        val y = new Down(new Face(Array(Int.MaxValue, face(0), face(2)), Int.MaxValue))
        val b = downTree.lower(y)
        if (b != null && b.face(1) == face(0) && b.face(2) == face(2)) {
        	face.ufParent = b.face
        }
      }
    }
    
    val newFaces = new scala.collection.mutable.HashSet[(Int, Int, Int)]
    blahFaces.foreach{face =>
      val parent = face.find
      if (face != parent) {
        for (f <- face.toArray if f != parent(0) && f != parent(2)) {
          val newFace = (parent(0), f, parent(2))
          newFaces += newFace
        }
        newFaces += ((face(0), face(1), face(2)))
      } else {
        newFaces += ((face(0), face(1), face(2)))
      }
    }

    newFaces.map(x => Array(x._1, x._2, x._3)).toArray

    //    //    val facesSorted = new scala.collection.mutable.TreeSet[Face]
    //
    //    val ts = new java.util.TreeSet[Face]
    //    faces.foreach(f => ts.add(new Face(f)))
    //    var f = ts.first()
    //    while (f != null) {
    //      val g = ts.higher(f)
    //      if (g != null && f(0) == g(0) && f(2) == g(2)) {
    //        val a = new Face(Array(f(0), f(1), g(1)))
    //        val b = new Face(Array(f(1), g(1), g(2)))
    //        if (ts.contains(a) && ts.contains(b)) {
    //          ts.remove(f)
    //          ts.remove(g)
    //        }
    //          
    //      }
    //
    //      f = ts.higher(f)
    //    }
    //
    //    val facesNew = ts.asScala.map(_.toArray).toArray
    //    println("Pruned faces size:  " + facesNew.size)
    //    facesNew
  }

  def chipotle(n: Int) = {
    val bottomFaces = (1 until (n - 1)).map(i => Array(0, i, i + 1)).toArray
    val topFaces = (0 until (n - 2)).map(i => Array(i, i + 1, n - 1)).toArray

    val faces = bottomFaces ++ topFaces

    val facesNew = pruneFaces(faces)

    val sf = new ScalarFunction(Array.fill(n)(Array.empty[Float]), facesNew, Array.range(0, n).map(_.toFloat))

    val ra = new ReebAlgorithm(sf)
    ra.run()
    val timeScala = ra.totalTime
    println("Time scala:  " + timeScala)
  }

  def triples(n: Int) = {
//    val facesOrig = Array.range(0, n).combinations(3).filter(face => math.random < .002).toArray
    val facesOrig = Array(Array(1, 2, 5), Array(2, 3, 5), Array(1, 3, 4))
    val facesPruned = pruneFaces(facesOrig)

    val edges = List(facesOrig, facesPruned).map { faces =>
      val sf = new ScalarFunction(Array.fill(n)(Array.empty[Float]), faces, Array.range(0, n).map(_.toFloat))

      val ra = new ReebAlgorithm(sf)
      ra.run()
      val timeScala = ra.totalTime
      println("Time scala:  " + timeScala)
      ra.reebEdges
    }

    val a = edges(0)
    val b = edges(1)

    if (edges(0).size != edges(1).size) {
      println
      println("orig:")
      a.foreach(e => println(e.mkString("\t(", ",", ")")))
      println("pruned:")
      b.foreach(e => println(e.mkString("\t(", ",", ")")))
      println("Oops")
    }

  }

  def compareVijay = {
    withBufferedWriter(new File("/dev/shm/compare.txt")) { bw =>
      pairs.foreach { pair =>
        (0 until 5).foreach { _ =>
          val f = new File(dir, pair._1)
          val timeVijay = runVijay(f, pair._2)

          val sc = SimplicialComplex.fromOffFile(f)
          val sf = new ScalarFunction(sc.vertices, sc.faces, sc.vertices(_)(pair._2))

          val ra = new ReebAlgorithm(sf)
          ra.run()
          val timeScala = ra.totalTime
          bw.write(f.getName + "\t" + timeVijay + "\t" + timeScala + "\n")
        }
      }
    }
  }

  def runVijay(offFile: File, funcCoord: Int): Float = {
    final class VijayOutputParser {
      var totalTime: Float = 0
      @inline
      def processLine(line: String): Unit = {
        if (line.startsWith("Time taken")) {
          totalTime = line.split("\\s+")(7).toFloat
        }
      }
    }

    // Write input.properties file for Vijay's algorithm
    val inputProps =
      """|# Input mesh loader type. Currently supported types are 
				 |# 	OFF - for 2D triangular meshes
				 |#	TET - for 3D tetrahedral meshes
				 |#	SIM - for higher dimensional simplicial meshes
				 |loader = OFF
				 |# Location of input file
				 |inputFile = %s
				 |# use 0 for given function, co-ordinate index to use a particular co-ordinate value
				 |inputFunction = %d
				 |# Where to store output Reeb graph. It is optional. Set to empty or remove option if you do not want the Reeb graph stored.  
				 |output= /dev/shm/vijay.off
				 |# set to false if the star of the vertex is to be used for BFS instead of triangle adjacencies
				 |adj = true
				 |""".stripMargin.format(offFile.getAbsolutePath(), funcCoord)

    withBufferedWriter(new File("/home/harveywi/Desktop/reconJava/input.properties")) { bw =>
      bw.write(inputProps)
    }

    val pb = Process("computeReebGraph.sh", new File("/home/harveywi/Desktop/reconJava/"))
    val outputParser = new VijayOutputParser
    pb.!(ProcessLogger(outputParser.processLine(_)))
    outputParser.totalTime
  }

  def check2 = {
    pairs.foreach(pair => {
      val f = new File(dir, pair._1)
      val coord = pair._2 match {
        case 0 => "-x"
        case 1 => "-y"
        case 2 => "-z"
        case 3 => "-w"
      }
      assert(f.exists)

      val sc = SimplicialComplex.fromOffFile(f)
      val sf = new ScalarFunction(sc.vertices, sc.faces, sc.vertices(_)(pair._2))

      val numTris = sf.faces.count(_.size == 3)
      println("Input tri count:  " + numTris)
      println("t log t:  " + numTris * math.log(numTris) / math.log(2))

      val reeb1 = Timing("Scala algorithm " + f.getName) {
        val ra = new ReebAlgorithm(sf)
        val arg = ra.run()
        val triHist = ra.triCounter.toArray.sortBy(_._2).reverse
        withBufferedWriter(new File("/dev/shm/triHist.txt")) { bw =>
          triHist.foreach { t =>
            bw.write(t._1 + ":\t" + t._2 + "\n")
          }
        }
        //        val edgeValences = ra.edgeValenceCounter.toArray.sortBy(_._2).reverse
        //        withBufferedWriter(new File("/dev/shm/edgeValences.txt")) { bw =>
        //          edgeValences.foreach { t =>
        //            bw.write(t._1 + ":\t" + t._2 + "\n")
        //          }
        //        }
        new SimplicialComplex(sc.vertices, arg.edgeSet().asScala.map { e =>
          if (sf.vc.compare(e.v1, e.v2) < 0) Array(e.v1, e.v2) else Array(e.v2, e.v1)
        }.toArray)
      }

      Timing("New algorithm " + f.getName) {
        val pb = Process(reebCmd2.format(coord, f.getAbsolutePath()))
        pb.!
      }
      val reeb2 = SimplicialComplex.fromOffFile(new File("out_aug.off"))

      def toTuple(arr: Array[Int]) = {
        val c = sf.vc.compare(arr(0), arr(1))
        if (c < 0) {
          (arr(0), arr(1))
        } else {
          (arr(1), arr(0))
        }
      }

      val edgesTrue = reeb1.faces.map(arr => toTuple(arr)).toSet
      val edgesRG = reeb2.faces.map(arr => toTuple(arr)).toSet
      val n1 = edgesTrue.diff(edgesRG).size
      val n2 = edgesRG.diff(edgesTrue).size

      assert(n1 == 0 && n2 == 0, { "Mismatch:  " + n1 + " " + n2 })
    })
    println("Done!")
  }

  def check = {

    pairs.foreach(pair => {
      val f = new File(dir, pair._1)
      val coord = pair._2 match {
        case 0 => "-x"
        case 1 => "-y"
        case 2 => "-z"
        case 3 => "-w"
      }
      assert(f.exists)
      Timing("Old algorithm " + f.getName) {
        val pb = Process(reebCmd1.format(coord, f.getAbsolutePath()))
        pb.!
      }
      val reeb1 = SimplicialComplex.fromOffFile(new File("out_aug.off"))

      Timing("New algorithm " + f.getName) {
        val pb = Process(reebCmd2.format(coord, f.getAbsolutePath()))
        pb.!
      }
      val reeb2 = SimplicialComplex.fromOffFile(new File("out_aug.off"))

      val sc = SimplicialComplex.fromOffFile(f)
      val sf = new ScalarFunction(sc.vertices, sc.faces, sc.vertices(_)(pair._2))

      def toTuple(arr: Array[Int]) = {
        val c = sf.vc.compare(arr(0), arr(1))
        if (c < 0) {
          (arr(0), arr(1))
        } else {
          (arr(1), arr(0))
        }
      }

      val edgesTrue = reeb1.faces.map(arr => toTuple(arr)).toSet
      val edgesRG = reeb2.faces.map(arr => toTuple(arr)).toSet
      val n1 = edgesTrue.diff(edgesRG).size
      val n2 = edgesRG.diff(edgesTrue).size

      assert(n1 == 0 && n2 == 0,
        "Mismatch:  " + n1 + " " + n2)
    })
    println("Done!")
  }
}