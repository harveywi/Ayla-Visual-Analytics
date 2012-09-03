package edu.osu.compgeom.topology

import java.io._
import scala.collection.mutable
import scala.annotation.tailrec
import scala.math.Ordered
import edu.osu.compgeom.util.HashUnionFind
import edu.osu.compgeom.util.HashUnionFind
import org.jgrapht.alg.DijkstraShortestPath

object ReebSlicer {

  class Tri(var v1: Int, var v2: Int, var v3: Int) {
    def e1 = (v1, v2)
    def e2 = (v1, v3)
    def e3 = (v2, v3)
    override def toString = (v1, v2, v3).toString
  }
  
  class Blah1(val vertSequence: mutable.ArrayBuffer[Int]) extends Exception
  class Blah2 extends Exception

  class Subset(val tris: List[Tri], val range: Range) {
//    require(range.end >= range.start)
  }
  
    def main(args: Array[String]): Unit = {
    	val sc = SimplicialComplex.fromOffFile(new File(args(1)))
    	val sf = new ScalarFunction(sc, sc.vertices(_)(args(0).toInt))
    	while (true) {
    	  try {
    	  	main2(sc, sf, args(0).toInt)
    	  } catch {
    	    case e: Blah2 => {println("Repeating")}
    	  }
    	}
    	
//    	var n = 20000
////    	var c = 0
//      while (true) {
//        val sc = SimplicialComplex.fromOffFile(new File("/dev/shm/sampled.off"))
//        println("N:  " + n)
//        val scSampled = new SimplicialComplex(sc.vertices, sc.faces.filter(_.size == 3).sortWith((f1, f2) => math.random < .5).take(n))
//        val sf = new ScalarFunction(scSampled, scSampled.vertices(_)(args(0).toInt))
//        try {
//          main2(scSampled, sf, args(0).toInt)
//        } catch {
//          case e: Blah1 => {
////            c = 0
//            println("Reproduced!")
//            n = (n * .97).toInt
//            scSampled.toOffFile(new File("/dev/shm/sampled.off"))
//            
//            val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/vertSequence.txt")))
//            e.vertSequence.foreach(v => bw.write(v + "\n"))
//            bw.flush
//            bw.close
////            System.exit(0)
//          }
//          case e: Blah2 => {
//            println("Repeating.")
////            c += 1
////            if (c == 15) {
////              c = 0
////              n = (n*1.2).toInt
////            }
//          }
//        }
//      }
    }

  def main2(sc: SimplicialComplex, sf: ScalarFunction, funcCoord: Int): Unit = {
//    if (args.size != 2) {
//      println("Usage:  cmd func_coord input_file")
//      System.exit(0)
//    }
//    val sc = SimplicialComplex.fromOffFile(new File(args(1)))
////        val sc = new SimplicialComplex(scTemp.vertices, scTemp.faces.sortWith((f1, f2) => math.random < .5).filter(_.size == 3).take(scTemp.faces.size / 2))
//    val sf = new ScalarFunction(sc, sc.vertices(_)(args(0).toInt))

    @inline
    def cv(i: Int, j: Int): Int = {
      val a = i % sc.vertices.size
      val b = j % sc.vertices.size
      return sf.vc.compare(a, b)
    }

    val reebEdges = new mutable.HashSet[(Int, Int)]
    reebEdges ++= sc.faces.filter(_.length == 2).map(e => if (cv(e(0), e(1)) < 0) (e(0), e(1)) else (e(1), e(0)))

    val vertsSorted = sf.vertices.indices.sortWith(cv(_, _) < 0)

    val initialTris = mutable.ListBuffer(sc.faces.flatMap(f => {
      if (f.length != 3)
        None
      else {
        val fSorted = f.sortWith(cv(_, _) < 0)
        Some(new Tri(fSorted(0), fSorted(1), fSorted(2)))
      }
    }): _*)

    val initialSubset = new Subset(initialTris.toList, Range(0, vertsSorted.size - 1))

    val stack = new mutable.Stack[Subset]
    stack.push(initialSubset)

    import scala.swing._
    import java.awt.geom._
    import java.awt.Color
    val canvas = new Panel {
      val lowerTris = new mutable.HashSet[Tri]
      val upperTris = new mutable.HashSet[Tri]
      val cutTris = new mutable.HashSet[Tri]
      val fc = funcCoord
      val xc = if (fc != 0) 0 else 1
      var p: Int = 0
      val minX = sf.vertices.map(_(xc)).min
      val maxX = sf.vertices.map(_(xc)).max
      val minY = sf.vertices.map(_(fc)).min
      val maxY = sf.vertices.map(_(fc)).max

      //            override def paint(g2d: Graphics2D): Unit = {}
      override def paint(g2d: Graphics2D): Unit = {
        g2d.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
        val w = g2d.getClipBounds.width
        val h = g2d.getClipBounds.height
        val oldPaint = g2d.getPaint
        val gradient = new java.awt.GradientPaint(0, 0, new Color(0x200772), 0, 1000, new Color(0x876ed7))
        g2d.setPaint(gradient)
        g2d.fillRect(0, 0, w, h)
        g2d.setPaint(oldPaint)

        def getCoords(idx: Int): Point = {
          val i = idx % sc.vertices.size

          val x = if (idx < sc.vertices.size) ((sf.vertices(i)(xc) - minX) / (maxX - minX) * (w-20)).round + 10 else w / 4
          val y = h - ((sf.vertices(i)(fc) - minY) / (maxY - minY) * (h-20)).round - 10
          new Point(x, y)
        }

        val fill = new Color(Color.gray.getRed, Color.gray.getGreen, Color.gray.getBlue, 120)
        def drawTri(t: Tri, fill: Color): Unit = {
          g2d.setColor(fill)
          val p1 = getCoords(t.v1)
          val p2 = getCoords(t.v2)
          val p3 = getCoords(t.v3)
          val xc = Array(p1.x, p2.x, p3.x)
          val yc = Array(p1.y, p2.y, p3.y)
          g2d.fillPolygon(xc, yc, 3);
          if (t.v1 != t.v2 && t.v2 != t.v3)
            g2d.setColor(Color.gray.darker)
          else
            g2d.setColor(fill)
          g2d.drawPolygon(xc, yc, 3);
        }

        val blue = new Color(Color.blue.getRed, Color.blue.getGreen, Color.blue.getBlue, 128)
        val yellow = new Color(Color.yellow.getRed, Color.yellow.getGreen, Color.yellow.getBlue, 128)
        val green = new Color(Color.green.getRed, Color.green.getGreen, Color.green.getBlue, 128)

        val white = new Color(255, 255, 255, 64)
        initialTris.foreach(t => {
          val fill: Color = if (lowerTris.contains(t)) blue else if (upperTris.contains(t)) yellow else if (cutTris.contains(t)) green else white
          drawTri(t, fill)
        })

        g2d.setColor(Color.white)
        //        reebEdges.foreach(e => {
        //          val p1 = getCoords(e._1)
        //          val p2 = getCoords(e._2)
        //          g2d.drawLine(p1.x, p1.y, p2.x, p2.y)
        //        })
        //
        //        g2d.setColor(Color.RED)
        //        badEdges.foreach(e => {
        //          val p1 = getCoords(e._1)
        //          val p2 = getCoords(e._2)
        //          g2d.drawLine(p1.x, p1.y, p2.x, p2.y)
        //        })

        g2d.setColor(Color.red)
        val pt = getCoords(p)
        if (funcCoord == 1 || funcCoord == 0)
          g2d.drawLine(0, pt.y, bounds.width, pt.y)
        else if (funcCoord == 0)
          g2d.drawLine(pt.x, 0, pt.x, bounds.height)
        g2d.drawOval(pt.x - 3, pt.y - 3, 7, 7)

        g2d.setColor(Color.green)
        def drawStringOpt(s: Int, x: Int, y: Int) = {
//          if (s == 7085 || s == 15286)
          	g2d.drawString(s.toString, x, y)
        }
        initialTris.foreach(t => {
          val p1 = getCoords(t.v1)
          val p2 = getCoords(t.v2)
          val p3 = getCoords(t.v3)
          drawStringOpt(t.v1, p1.getX.toInt, p1.getY.toInt)
          drawStringOpt(t.v2, p2.getX.toInt, p2.getY.toInt)
          drawStringOpt(t.v3, p3.getX.toInt, p3.getY.toInt)
        })
        //          			g2d.setColor(Color.red)
        //          			g2d.drawLine(getCoords(8).x, getCoords(8).y, getCoords(9).x, getCoords(9).y)
      }
    }
    val frame = new Frame {
      title = "Test"
      visible = true
      contents = canvas
      size = new Dimension(800, 800)
    }

    //      def checkGroundTruth() = {
    //        val checkTris = initialTris.filter(t => t.v1 != t.v2 && t.v2 != t.v3).map(t => Array(t.v1, t.v2, t.v3)).toSet.toArray
    //        val checkTriEdges = checkTris.flatMap(t => List((t(0), t(1)), (t(0), t(2)), (t(1), t(2)))).toSet
    //        val checkReebEdges = (reebEdges.map(e => if (sf.vc.compare(e._1, e._2) < 0) Array(e._1, e._2) else Array(e._2, e._1)) ++ initialTris.flatMap(t => {
    //          if (t.v1 == t.v3)
    //            None
    //          else if (t.v1 == t.v2 && !checkTriEdges.contains((t.v2, t.v3)))
    //            Some((t.v2, t.v3))
    //          else if (t.v2 == t.v3 && !checkTriEdges.contains((t.v1, t.v2)))
    //            Some((t.v1, t.v2))
    //          else
    //            None
    //        }).distinct.map(t => Array(t._1, t._2))).toArray
    //        val scCheck = new SimplicialComplex(sc.vertices, checkTris ++ checkReebEdges)
    //        val sfCheck = new ScalarFunction(scCheck, sc.vertices(_)(args(0).toInt))
    //        //      scCheck.toOffFile(new File("/dev/shm/check.off"))
    //        val rgCheck = (new ReebAlgorithm(sfCheck)).run
    //        import scala.collection.JavaConversions._
    //        val s1 = trueReeb.edgeSet.map(e => {
    //          (e.v1, e.v2)
    //        })
    //        val s2 = rgCheck.edgeSet.map(e => {
    //          (e.v1, e.v2)
    //        })
    //        if (s1 != s2) {
    //          println(s1.size)
    //          println(s2.size)
    //          println("GT - result:  " + s1.diff(s2))
    //          println("Result - GT:  " + s2.diff(s1))
    //          println(reebEdges.contains(6969, 11582))
    //          println(initialTris.find(t => t.v1 == 6969 && t.v3 == 11582 && (t.v1 == t.v2 || t.v2 == t.v3)))
    //
    //          //        println(trueReeb.edgesOf(8229))
    //          //        println(rgCheck.edgesOf(8229))
    //
    //          val scReeb = new SimplicialComplex(sc.vertices, trueReeb.edgeSet.map(e => Array(e.v1, e.v2)).toArray)
    //          scReeb.toOffFile(new File("/dev/shm/true.off"))
    //
    //          val scErr = new SimplicialComplex(sc.vertices, rgCheck.edgeSet.map(e => Array(e.v1, e.v2)).toArray)
    //          scErr.toOffFile(new File("/dev/shm/err.off"))
    //          throw new Blah1
    //          assert(false)
    //          val t = 3
    //        }
    //      }

    println("Go!")
    val ps = mutable.Stack(3)
    val t1 = System.currentTimeMillis
    val partitionIndices = mutable.Stack(15,8,0,3,1,2,5,4,6,7,9,13,10,11,12,14,17,16,18,19)
//    val partitionIndices = new mutable.ArrayBuffer[Int]
    while (!stack.isEmpty) {
      val subset = stack.pop
      //val partitionIdx = subset.range.start + (subset.range.end - subset.range.start) / 2
//      val partitionIdx = subset.range.start + ((subset.range.end - subset.range.start) * math.random).toInt
      val partitionIdx = partitionIndices.pop
//      partitionIndices += partitionIdx
      val p = vertsSorted(partitionIdx)
//      println("p:  " + p)

//            Thread.sleep(1000)
      canvas.upperTris.clear()
      canvas.lowerTris.clear()
      canvas.cutTris.clear()

      canvas.p = p
      canvas.repaint

      val upperTris = new mutable.ListBuffer[Tri]
      val lowerTris = new mutable.ListBuffer[Tri]
      val cutTris = new mutable.ListBuffer[Tri]
      val ufPlus = new HashUnionFind[(Int, Int)]
      val ufMinus = new HashUnionFind[(Int, Int)]

      subset.tris.foreach(t => {
        if (cv(p, t.v1) < 0) {
          upperTris += t
        } else if (cv(t.v3, p) < 0) {
          lowerTris += t
        } else {
          val v2c = cv(t.v2, p)

          // update bounds
          if (v2c < 0) {
            ufMinus.union(t.e2, t.e3)
            val x = cv(t.v3, p)
            if (x != 0)
              ufPlus.union(t.e2, t.e3)
          } else if (v2c > 0) {
            val x = cv(t.v1, p)
            if (x != 0)
              ufMinus.union(t.e1, t.e2)
            ufPlus.union(t.e1, t.e2)
          } else {
            ufMinus.union(t.e1, t.e2)
            ufPlus.union(t.e2, t.e3)
          }

          cutTris += t
        }
      })

      canvas.upperTris ++= upperTris
      canvas.lowerTris ++= lowerTris
      canvas.cutTris ++= cutTris
      //      canvas.repaint()

      val degenerateTris = new mutable.ListBuffer[Tri]
      
      def countCC() = {}
      def countCC2() = {
        val cutTris = new mutable.ListBuffer[Tri]
        val ufPlus = new HashUnionFind[(Int, Int)]
        val ufMinus = new HashUnionFind[(Int, Int)]
        val p = 1
        initialTris.foreach(t => {
	        if (cv(p, t.v1) < 0) {
	          
	        } else if (cv(t.v3, p) < 0) {
	          
	        } else {
	          val v2c = cv(t.v2, p)
	
	          // update bounds
	          if (v2c < 0) {
	            ufMinus.union(t.e2, t.e3)
	            val x = cv(t.v3, p)
	            if (x != 0)
	              ufPlus.union(t.e2, t.e3)
	          } else if (v2c > 0) {
	            val x = cv(t.v1, p)
	            if (x != 0)
	              ufMinus.union(t.e1, t.e2)
	            ufPlus.union(t.e1, t.e2)
	          } else {
	            ufMinus.union(t.e1, t.e2)
	            ufPlus.union(t.e2, t.e3)
	          }
	
	          cutTris += t
	        }
	      })
	      val numMinus = cutTris.filter(_.v1 != p).map(t => ufMinus.find(t.e2)).toSet.size
	      val numPlus = cutTris.filter(_.v3 != p).map(t => ufPlus.find(t.e2)).toSet.size
	      println("Through 7:  " + numMinus + " below and " + numPlus + " above.")
      }
      countCC()

      // Consider the contours above and below p.  Label each of them uniquely with integers starting
      // with 1.
      // Find out which contours pass through a triangle containing vertex p, and call this set P.  This partitions the set of contours
      // C into P and Q.
      // For each (-,+) pair of artificial vertices in Q:
      //   Add a degenerate triangle from - to +
      // For each - vertex in P:
      //   Add a degenerate triangle from - to p
      // For each + vertex in P:
      //   Add a degenerate triangle from p to +

      // Now, slide/cut triangles away from p.
      // For each triangle t:
      // Case 1 (t.v2 < p):
      //   Set t.v3 to artificial point of +contour through t.e2
      // Case 2 (t.v2 > p):
      //   Set t.v1 to artificial point of -contour through t.e2
      // Case 3 (t.v2 == p):
      //   Split t into to triangles t+ and t-
      //   Let p+ be the artificial point of the +contour through t.e2
      //   Let p- be the artificial point of the -contour through t.e2
      //   t+ = (p+, p+, t.v3)
      //   t- = (t.v1, p-, p-)

      // Some problems can occur.  Namely, consider some contour e.g. c+ in Q.
      // If all of the triangles passing through c+ have their middle vertices below p,
      // then we will slide all of those triangles down below p, and c+ will have no support.
      // Thus, we need to create a support in the form of a degenerate triangle from c+ to
      // t.v3 for some triangle t passing through c+.
      // (NOTE:  Must confirm that there will be exactly one vertex above c+)
      //
      // The same reasoning follows for some contour c- in Q.
      
      def vNewIdx(contourID: Int) = sc.vertices.size*contourID + p

      val numMinus = cutTris.filter(_.v1 != p).map(t => ufMinus.find(t.e2)).toSet.size
      val numPlus = cutTris.filter(_.v3 != p).map(t => ufPlus.find(t.e2)).toSet.size
//      println(p + " has " + numMinus + " lower and " + numPlus + " upper.")
      val counterMinus = Iterator.from(1)
      val counterPlus = Iterator.from(numMinus + 1)
      val componentToIDPlus = new mutable.HashMap[(Int, Int), Int]
      val componentToIDMinus = new mutable.HashMap[(Int, Int), Int]
      val supported = new mutable.HashSet[Int]
      val P = new mutable.HashSet[Int]
      val scaffoldEdges = new mutable.HashSet[(Int, Int)]
      cutTris.foreach(t => {
        val e2 = t.e2
        if (t.v1 == p) {
          val cPlus = ufPlus.find(e2)
          val plusID = componentToIDPlus.getOrElseUpdate(cPlus, counterPlus.next)
          P += plusID
          supported += plusID
          scaffoldEdges += new Tuple2(p, vNewIdx(plusID))
        } else if (t.v3 == p) {
          val cMinus = ufMinus.find(e2)
          val minusID = componentToIDMinus.getOrElseUpdate(cMinus, counterMinus.next)
          P += minusID
          supported += minusID
          scaffoldEdges += new Tuple2(vNewIdx(minusID), p)
        } else if (t.v2 == p) {
          val cPlus = ufPlus.find(e2)
          val plusID = componentToIDPlus.getOrElseUpdate(cPlus, counterPlus.next)
          val cMinus = ufMinus.find(e2)
          val minusID = componentToIDMinus.getOrElseUpdate(cMinus, counterMinus.next)
          P += plusID
          P += minusID
          supported += plusID
          supported += minusID
          scaffoldEdges += new Tuple2(vNewIdx(minusID), p)
          scaffoldEdges += new Tuple2(p, vNewIdx(plusID))
        } else {
          val cPlus = ufPlus.find(e2)
          val plusID = componentToIDPlus.getOrElseUpdate(cPlus, counterPlus.next)
          val cMinus = ufMinus.find(e2)
          val minusID = componentToIDMinus.getOrElseUpdate(cMinus, counterMinus.next)
          val v2c = cv(t.v2, p)
          if (v2c < 0) {
            supported += minusID
          } else if (v2c > 0) {
            supported += plusID
          } else {
            throw new RuntimeException
          }
        }
        countCC()
      })
      
      cutTris.filter(t => t.v1 != p && t.v2 != p && t.v3 != p).foreach(t => {
        val e2 = t.e2
        val cPlus = ufPlus.find(e2)
        val plusID = componentToIDPlus.getOrElseUpdate(cPlus, counterPlus.next)
        val cMinus = ufMinus.find(e2)
        val minusID = componentToIDMinus.getOrElseUpdate(cMinus, counterMinus.next)
        if (!P.contains(plusID) && !P.contains(minusID)) {
          scaffoldEdges += new Tuple2(vNewIdx(minusID), vNewIdx(plusID))
        } else {
          assert(P.contains(plusID) && P.contains(minusID))
        }
      })

      // Add degenerate triangles when support is missing
      cutTris.filter(t => t.v1 != p && t.v2 != p && t.v3 != p).foreach(t => {
        val e2 = t.e2
        val v2c = cv(t.v2, p)
        if (v2c < 0) {
          val cPlus = ufPlus.find(e2)
          val plusID = componentToIDPlus(cPlus)
          if (!supported.contains(plusID)) {
            supported += plusID
            val vNew = vNewIdx(plusID)
            initialTris += new Tri(vNew, vNew, t.v3)
          }
        } else if (v2c > 0) {
          val cMinus = ufMinus.find(e2)
          val minusID = componentToIDMinus(cMinus)
          if (!supported.contains(minusID)) {
            supported += minusID
            val vNew = vNewIdx(minusID)
            initialTris += new Tri(t.v1, vNew, vNew)
          }
        } else {
          throw new RuntimeException
        }
        countCC()
      })

      // Now slide (or cut) each triangle
      cutTris.foreach(t => {
        val e2 = t.e2
        val x = cv(t.v1, p)
        val y = cv(t.v3, p)
        if (t.v1 == p) {
          val cPlus = ufPlus.find(e2)
          val plusID = componentToIDPlus(cPlus)
          val vNew = vNewIdx(plusID)
          t.v1 = vNew
          upperTris += t
        } else if (t.v3 == p) {
          val cMinus = ufMinus.find(e2)
          val minusID = componentToIDMinus(cMinus)
          val vNew = vNewIdx(minusID)
          t.v3 = vNew
          lowerTris += t
        } else {
        	val v2c = cv(t.v2, p)
        	if (v2c < 0) {
        	  val cMinus = ufMinus.find(e2)
        	  val minusID = componentToIDMinus(cMinus)
        	  val vNew = vNewIdx(minusID)
        	  t.v3 = vNew
        	  lowerTris += t
        	} else if (v2c > 0) {
        	  val cPlus = ufPlus.find(e2)
        	  val plusID = componentToIDPlus(cPlus)
        	  val vNew = vNewIdx(plusID)
        	  t.v1 = vNew
        	  upperTris += t
        	} else {
        	  val cPlus = ufPlus.find(e2)
        	  val plusID = componentToIDPlus(cPlus)
        	  val vUpper = vNewIdx(plusID)
        	  
        	  val cMinus = ufMinus.find(e2)
        	  val minusID = componentToIDMinus(cMinus)
        	  val vLower = vNewIdx(minusID)
        	  
        	  val tLower = new Tri(t.v1, vLower, vLower)
        	  lowerTris += tLower
        	  initialTris += tLower
        	  
        	  t.v1 = vUpper
        	  t.v2 = vUpper
        	  upperTris += t
        	}
        }
        countCC()
      })
      
      // Create degenerate triangles for all of the scaffolding
      scaffoldEdges.foreach(e => {
        val tNew = new Tri(e._1, e._1, e._2)
        initialTris += tNew
      })
      countCC()
      if (subset.range.start == subset.range.end) {

      } else if (subset.range.end - subset.range.start == 1) {
        val lowerSubset = new Subset(lowerTris.toList, Range(subset.range.start, subset.range.start))
        val upperSubset = new Subset(upperTris.toList, Range(subset.range.end, subset.range.end))
        if (upperSubset.range.end >= upperSubset.range.start)
        	stack.push(upperSubset)
      } else {
        val lowerSubset = new Subset(lowerTris.toList, Range(subset.range.start, partitionIdx - 1))
        val upperSubset = new Subset(upperTris.toList, Range(partitionIdx + 1, subset.range.end))
        if (upperSubset.range.end >= upperSubset.range.start)
        	stack.push(upperSubset)
        if (lowerSubset.range.end >= lowerSubset.range.start)
        	stack.push(lowerSubset)
      }
    }

    initialTris.foreach(t => {
      if (t.v1 != t.v2)
        reebEdges += Tuple2(t.v1, t.v2)
      if (t.v2 != t.v3)
        reebEdges += Tuple2(t.v2, t.v3)
    })

    import org.jgrapht.graph._
    val g = new SimpleDirectedGraph[Int, DefaultEdge](classOf[DefaultEdge])
    reebEdges.foreach {
      case (i, j) => {
        g.addVertex(i)
        g.addVertex(j)
        g.addEdge(i, j)
      }
    }
    
    import scala.collection.JavaConversions._
    val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/join.dot")))
			bw.write("digraph G {\n")
			g.edgeSet().foreach(e => {
			  val v1 = g.getEdgeSource(e)
			  val v2 = g.getEdgeTarget(e)
			  bw.write(v1 + " -> " + v2 + "\n")
			})
			bw.write("}\n")
			bw.flush
			bw.close
			
//		val stack = new mutable.Stack[Int]
//    g.vertexSet().filter(v => {
//      v >= sc.vertices.size
//    })

    g.vertexSet().filter(_ >= sc.vertices.size).foreach(v => {
      val edgesIn = g.incomingEdgesOf(v).toSeq
      val edgesOut = g.outgoingEdgesOf(v).toSeq
      assert(edgesIn.size <= 1 && edgesOut.size <= 1)
      if (edgesIn.size == 0) {
        g.removeEdge(edgesOut(0))
      } else if (edgesOut.size == 0) {
        g.removeEdge(edgesIn(0))
      } else {
        val e1 = edgesIn(0)
	      val e2 = edgesOut(0)
	      val i = g.getEdgeSource(e1)
	      val j = g.getEdgeTarget(e2)
	      g.removeEdge(e1)
	      g.removeEdge(e2)
	      g.addEdge(i, j)
      }
      
      g.removeVertex(v)
    })
    reebEdges.clear()
    g.edgeSet.foreach(e => {
      val v1 = g.getEdgeSource(e)
      val v2 = g.getEdgeTarget(e)
      reebEdges += new Tuple2(v1, v2)
    })

    canvas.repaint()

    val t2 = System.currentTimeMillis

    val totalTime = t2 - t1
    println("Total time:  " + (t2 - t1) / 1000.0)

    val trueReeb = (new ReebAlgorithm(sf)).run()
    val s1 = trueReeb.edgeSet.map(e => (e.v1, e.v2)).toSet
    val s2 = reebEdges
    if (s1 == s2) {
      println("Success!")
      throw new Blah2
      System.exit(0)
    } else {
      println(s1.size)
      println(s2.size)
      println("GT - result:  " + s1.diff(s2))
      println("Result - GT:  " + s2.diff(s1))
      println(partitionIndices.mkString("", ",", "\n"))
      throw new Blah1(null)
    }
    
    val newFaces = reebEdges.map(e => Array(e._1, e._2)).toArray
    val scNew = new SimplicialComplex(sc.vertices.map(v => v.take(3)), newFaces)
    scNew.toOffFile(new File("/dev/shm/test.off"))
  }

}