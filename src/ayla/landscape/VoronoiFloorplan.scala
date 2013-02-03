/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.landscape

import com.sun.j3d.utils.geometry.Primitive
import javax.media.j3d.Material
import javax.media.j3d.TransparencyAttributes
import javax.media.j3d.ColoringAttributes
import com.sun.j3d.utils.geometry.Sphere
import javax.media.j3d.Appearance
import javax.vecmath.Vector3f
import javax.media.j3d.TransformGroup
import javax.media.j3d.Transform3D
import javax.media.j3d.BranchGroup
import com.sun.j3d.utils.geometry.Stripifier
import com.sun.j3d.utils.geometry.NormalGenerator
import com.sun.j3d.utils.geometry.GeometryInfo
import javax.media.j3d.Shape3D
import javax.media.j3d.IndexedGeometryArray
import java.awt.geom.PathIterator
import javax.media.j3d.IndexedTriangleArray
import java.awt.geom.Line2D
import java.awt.geom.Area
import java.awt.geom.Path2D
import java.awt.Polygon
import ayla.geometry.ct._
import scala.collection.mutable.ArrayBuffer
import java.awt.geom.Rectangle2D
import java.awt.geom.GeneralPath
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.Color
import java.awt.geom.AffineTransform
import javax.media.jai.JAI
import org.jgrapht.graph._
import org.jgrapht.alg._
import javax.media.j3d.GeometryArray
import javax.media.j3d.TriangleArray
import javax.vecmath.{ Color3f, Point3f }
import scala.collection.JavaConversions._
import org.jgrapht.graph.{ DefaultDirectedGraph, DefaultEdge }
import scala.collection.mutable.Stack
import java.awt.geom.Point2D
import javax.media.j3d.LineArray
import scala.collection.mutable.MultiMap
import ayla.util.Timing
import javax.media.j3d.Geometry

@SerialVersionUID(1L)
class VoronoiFloorplan(val floorplan: Floorplan[Array[Point2D]],
  val topoComponents: Array[Shape3D],
  val getCriticalNodeHeight: Int => Double,
  val sphereBranchGroup: BranchGroup,
  val lineArray: LineArray,
  val contourTreeCriticalNodeToTerrainVerts: Map[Int, ArrayBuffer[Int]],
  val contourTreeNoncriticalNodesToTerrainVerts: Map[Int, ArrayBuffer[NoncriticalPointTerrainPos]],
  val terrainVertices: Array[Point3f],
  val terrainGraph: SimpleWeightedGraph[Int, DefaultWeightedEdge],
  val nodeAtInfinity: ContourTreeNode) extends Serializable

object VoronoiFloorplan {
  val terrainHeight = .7f
  //  val terrainHeight = 1f

  def apply(
    bspEnsemble: BSPTreeEnsemble,
    ct: ContourTree,
    epsilon: Double = 1e-4,
    delta: Double = 1e-5,
    pctChange: Double = 1d): VoronoiFloorplan = {

    val outerBdry = Array((0, 0), (1, 0), (1, 1), (0, 1)).map { case (x, y) => new Point2D.Double(x, y).asInstanceOf[Point2D] }

    //	  val outerBdry = (0 until 72).map(i => {
    //	    val theta = 2 * math.Pi * i / 72.0
    //	    new Point2D.Double(.5 + math.cos(theta), .5 + math.sin(theta)).asInstanceOf[Point2D]
    //	  }).toArray

    class VoronoiSplitRule extends SplitRule {
      def getNextSplit = this
    }

    // For calculating the height of each vertex (between 0 and 1)
    val funcVals = ct.nodesContracted.map(n => ct.scalarFunction.getFuncVal(n.vertex))
    val minFuncVal = funcVals.foldLeft(Float.PositiveInfinity)(math.min)
    val maxFuncVal = funcVals.foldLeft(Float.NegativeInfinity)(math.max)
    val rangeFuncVal = maxFuncVal - minFuncVal

    def getVertexHeight(vertIdx: Int): Double = {
      val funcVal = ct.scalarFunction.getFuncVal(vertIdx)
      val height = terrainHeight * (funcVal - minFuncVal) / rangeFuncVal
      height
    }

    //		def scale(x: Double, y: Double, minX: Double, maxX: Double, minY: Double, maxY: Double): java.awt.Point = {
    //			val ix = 50 + (900* (x - minX) / (maxX - minX)).round.toInt
    //			val iy = 50 + (900 * (y - minY) / (maxY - minY)).round.toInt
    //			return new java.awt.Point(ix, iy)
    //		}
    //		val debugImg = new BufferedImage(2000, 2000, BufferedImage.TYPE_3BYTE_BGR)
    //		val g2d = debugImg.getGraphics.asInstanceOf[Graphics2D]
    //		def saveImage(fileName: String): Unit = {
    //			JAI.create("filestore", debugImg, "/dev/shm/" + fileName, "PNG"); 
    //			System.gc()
    //		}

    val floorplan = Floorplan[Array[Point2D]](bspEnsemble, scaleContour,
      splitContourCirclePartition, outerBdry, new VoronoiSplitRule, _ => "")
    //		val floorplan = Floorplan[I, D, Array[Point2D]](bspEnsemble, scaleContour, splitContour(_, _, _, epsilon, delta, pctChange), outerBdry, new VoronoiSplitRule, _ => "")

    val (topoComponents, sphereBranchGroup, entireIndexedTriArray, terrainVertToContourTreeNode) = buildTriangleArray(floorplan, getVertexHeight)

    // make sure all contour tree edges were touched
    val visitedEdges = new scala.collection.mutable.HashSet[ContourTreeEdge]
    val stack = new Stack[FloorplanElem[Array[Point2D]]]
    stack.pushAll(floorplan.rootElems)

    while (!stack.isEmpty) {
      val floorplanElem = stack.pop
      visitedEdges += floorplanElem.bspNode.ctEdge
      stack.pushAll(floorplanElem.children)
    }

    assert(ct.criticalNodeToIncidentEdges.values.flatten.forall(visitedEdges.contains(_)))

    // Build a weighted graph with the same vertices/edges as the terrain.  Edges will have weight
    // equal to the length of the edge.
    val g = new SimpleWeightedGraph[Int, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    (0 until entireIndexedTriArray.getVertexCount()).foreach(g.addVertex(_))
    val vertices = Array.fill[Point3f](entireIndexedTriArray.getVertexCount())(new Point3f)
    entireIndexedTriArray.getCoordinates(0, vertices)
    val indices = new Array[Int](entireIndexedTriArray.getIndexCount())
    entireIndexedTriArray.getCoordinateIndices(0, indices)
    Timing("building terrain graph") {
      val triPairs = Array((0, 1), (0, 2), (1, 2))
      (0 until indices.size by 3).foreach { ofs =>
        triPairs.foreach {
          case (i, j) =>
            val e = g.addEdge(indices(ofs + i), indices(ofs + j))
            val w = vertices(indices(ofs + i)).distance(vertices(indices(ofs + j)))
            g.setEdgeWeight(e, w)
        }
      }
    }
    
    println("Num edges before update:  " + g.edgeSet.size)

    Timing("updating terrain graph") {
      val mmap = new scala.collection.mutable.HashMap[Point3f, scala.collection.mutable.ArrayBuffer[Int]]
      vertices.zipWithIndex.foreach {
        case (v, i) => {
          val list = mmap.getOrElseUpdate(v, new scala.collection.mutable.ArrayBuffer[Int])
          list += i
        }
      }

      mmap.filter(_._2.size > 1).foreach(entry => {
        entry._2.sliding(2).foreach(pair => {
          val e = g.addEdge(pair(0), pair(1))
          if (e != null)
            g.setEdgeWeight(e, 0)
        })
      })
    }
    
    println("Terrain graph statistics:")
    println("\tVertices:  " + g.vertexSet.size)
    println("\tEdges:  " + g.edgeSet.size)

    val lineArray = new LineArray(2, GeometryArray.COORDINATES)

    val contourTreeCriticalNodeToTerrainVerts = new scala.collection.mutable.HashMap[Int, scala.collection.mutable.ArrayBuffer[Int]]
    terrainVertToContourTreeNode.zipWithIndex.foreach {
      case (contourTreeNodeID, terrainVertID) => {
        val criticalNodes = contourTreeCriticalNodeToTerrainVerts.getOrElseUpdate(contourTreeNodeID, new scala.collection.mutable.ArrayBuffer[Int])
        criticalNodes += terrainVertID
      }
    }

    ct.nodesContracted.foreach { n =>
      contourTreeCriticalNodeToTerrainVerts.get(n.vertex) match {
        case Some(criticalNodes) => {}
        case None => {
          println("Note:  Found")
          contourTreeCriticalNodeToTerrainVerts(n.vertex) = new scala.collection.mutable.ArrayBuffer[Int]
        }
      }
    }

    val contourTreeNoncriticalNodeToTerrainVerts = new scala.collection.mutable.HashMap[Int, scala.collection.mutable.ArrayBuffer[NoncriticalPointTerrainPos]]

    ct.criticalNodeToIncidentEdges.values.flatten.toArray.distinct.foreach { e =>
      val vSet1 = contourTreeCriticalNodeToTerrainVerts(e.n1.vertex)
      val vSet2 = contourTreeCriticalNodeToTerrainVerts(e.n2.vertex)
      val f1 = ct.scalarFunction.getFuncVal(e.n1.vertex)
      val f2 = ct.scalarFunction.getFuncVal(e.n2.vertex)
      vSet1.foreach { v1 =>
        var minPair: Tuple2[Int, Int] = null
        var minDist = Double.MaxValue
        vSet2.foreach { v2 =>
          val dist = vertices(v1).distanceSquared(vertices(v2))
          if (dist < minDist) {
            minDist = dist
            minPair = (v1, v2)
          }
        }
        if (minPair != null) {
          e.noncriticalNodes.foreach { n =>
            val f = ct.scalarFunction.getFuncVal(n.vertex)
            val alpha = if (f2 - f1 != 0) (f - f1) / (f2 - f1) else 0
            val x1 = vertices(minPair._1)
            val x2 = vertices(minPair._2)
            val p = new Point3f()
            p.x = x1.x + alpha * (x2.x - x1.x)
            p.y = x1.y + alpha * (x2.y - x1.y)
            p.z = x1.z + alpha * (x2.z - x1.z)
            val verts = contourTreeNoncriticalNodeToTerrainVerts.getOrElseUpdate(n.vertex, new scala.collection.mutable.ArrayBuffer[NoncriticalPointTerrainPos])
            verts += new NoncriticalPointTerrainPos(minPair._1, minPair._2, p)
//            contourTreeNoncriticalNodeToTerrainVerts(n.vertex) = p
          }
        }
      }
    }

    println("Success!")
    return new VoronoiFloorplan(floorplan, topoComponents, getVertexHeight,
      sphereBranchGroup, lineArray, contourTreeCriticalNodeToTerrainVerts.toMap, contourTreeNoncriticalNodeToTerrainVerts.toMap, vertices, g, bspEnsemble.nodeAtInfinity)
  }

  def buildTriangleArray(floorplan: Floorplan[Array[Point2D]], getVertexHeight: Int => Double): (Array[Shape3D], BranchGroup, IndexedTriangleArray, Array[Int]) = {

    val verts = new ArrayBuffer[Point3f]
    val stack = new Stack[FloorplanElem[Array[Point2D]]]

    val terrainVertToContourTreeNode = new ArrayBuffer[Int]

    class Subset(val range: Range, val e: ContourTreeEdge)
    val subsets = new scala.collection.mutable.ArrayBuffer[Subset]

    val sphereBranchGroup = new BranchGroup()

    stack.pushAll(floorplan.rootElems)
    while (!stack.isEmpty) {
      val floorplanElem = stack.pop
      val contour = floorplanElem.contour
      if (contour.isEmpty) {
        stack.pushAll(floorplanElem.children)
      } else {
        val hOuter = getVertexHeight(floorplanElem.bspNode.outerNode.vertex)
        val hInner = getVertexHeight(floorplanElem.bspNode.innerNode.vertex)
        //			val (colorOuter, colorInner) = (edgeColor(floorplanElem.bspNode.ctEdge, 1), edgeColor(floorplanElem.bspNode.ctEdge, 0))
        val numNewVerts = if (floorplanElem.children.isEmpty) {
          // Triangulate terminal region
          //				println("Triangulating terminal region")
          triangulateTerminalPolygon(contour, verts, hOuter, hInner, sphereBranchGroup, floorplanElem.bspNode.ctEdge.noncriticalNodes, getVertexHeight, terrainVertToContourTreeNode,
            floorplanElem.bspNode.outerNode.vertex, floorplanElem.bspNode.innerNode.vertex)
        } else {
          // Triangulate an annulus
          val sumChildAreas = floorplanElem.bspNode.children.map(_.sumArea).sum
          val scaleFactor = math.sqrt(sumChildAreas) / math.sqrt(floorplanElem.bspNode.sumArea)
          val innerContour = scaleContour(contour, scaleFactor)

          //				println("Scale factor:  " + scaleFactor)
          stack.pushAll(floorplanElem.children)
          triangulateAnnulus(contour, innerContour, verts, hOuter, hInner, sphereBranchGroup, floorplanElem.bspNode.ctEdge.noncriticalNodes, getVertexHeight, terrainVertToContourTreeNode,
            floorplanElem.bspNode.outerNode.vertex, floorplanElem.bspNode.innerNode.vertex)
        }

        val subset = new Subset(Range(verts.size - numNewVerts, verts.size), floorplanElem.bspNode.ctEdge)
        subsets += subset

        /*
				val ita = new IndexedTriangleArray(verts.size, GeometryArray.COORDINATES | GeometryArray.COLOR_4 | GeometryArray.NORMALS, verts.size)
				ita.setCoordinates(0, verts.toArray)
				ita.setCoordinateIndices(0, verts.indices.toArray)
				
				val gi = new GeometryInfo(ita)
				gi.recomputeIndices
				gi.compact
				val ng = new NormalGenerator
				ng.generateNormals(gi)
				val st = new Stripifier
				st.stripify(gi)
				val ta = gi.getGeometryArray
				ta.setCapability(GeometryArray.ALLOW_COLOR_READ)
				ta.setCapability(GeometryArray.ALLOW_COLOR_WRITE)
				ta.setCapability(IndexedGeometryArray.ALLOW_COLOR_INDEX_READ)
				ta.setCapability(IndexedGeometryArray.ALLOW_COLOR_INDEX_WRITE)
				ta.setCapability(IndexedGeometryArray.ALLOW_NORMAL_INDEX_READ)
				ta.setCapability(IndexedGeometryArray.ALLOW_NORMAL_INDEX_WRITE)
				ta.setCapability(GeometryArray.ALLOW_NORMAL_READ)
				ta.setCapability(GeometryArray.ALLOW_NORMAL_WRITE)
				
				val s3d = new Shape3D(ta)
				s3d.setPickable(true)
				s3d.setUserData(floorplanElem.bspNode.ctEdge)
				
				s3d.setCapability(Shape3D.ALLOW_APPEARANCE_READ)
				s3d.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE)
				
				allComponents += s3d
				verts.clear
				*/
      }
    }

    val ita = new IndexedTriangleArray(verts.size, GeometryArray.COORDINATES | GeometryArray.COLOR_4 | GeometryArray.NORMALS, verts.size)
    ita.setCoordinates(0, verts.toArray)
    ita.setCoordinateIndices(0, verts.indices.toArray)

    val gi = new GeometryInfo(ita)
    gi.recomputeIndices
    val ng = new NormalGenerator
    ng.generateNormals(gi)
    gi.unindexify
    val normals = gi.getNormals

    val allComponents = subsets.map(subset => {
      val v = subset.range.map(verts(_))
      val ita = new IndexedTriangleArray(v.size, GeometryArray.COORDINATES | GeometryArray.COLOR_4 | GeometryArray.NORMALS, v.size)
      ita.setCoordinates(0, v.toArray)
      ita.setCoordinateIndices(0, v.indices.toArray)

      val n = subset.range.map(normals(_))
      ita.setNormals(0, n.toArray)
      ita.setNormalIndices(0, n.indices.toArray)

      val gi = new GeometryInfo(ita)
      gi.recomputeIndices
      gi.compact
      val st = new Stripifier
      st.stripify(gi)
      val ta = gi.getGeometryArray
      ta.setCapability(GeometryArray.ALLOW_COLOR_READ)
      ta.setCapability(GeometryArray.ALLOW_COLOR_WRITE)
      ta.setCapability(IndexedGeometryArray.ALLOW_COLOR_INDEX_READ)
      ta.setCapability(IndexedGeometryArray.ALLOW_COLOR_INDEX_WRITE)

      ta.setCapability(GeometryArray.ALLOW_COORDINATE_READ)
      ta.setCapability(GeometryArray.ALLOW_COORDINATE_WRITE)
      ta.setCapability(GeometryArray.ALLOW_NORMAL_READ)
      ta.setCapability(GeometryArray.ALLOW_NORMAL_WRITE)

      ta.setCapability(Geometry.ALLOW_INTERSECT)
      ta.setCapability(GeometryArray.ALLOW_FORMAT_READ)
      ta.setCapability(GeometryArray.ALLOW_COUNT_READ)
      ta.setCapability(GeometryArray.ALLOW_COORDINATE_READ)

      //			ta.setCapability(IndexedGeometryArray.ALLOW_NORMAL_INDEX_READ)
      //			ta.setCapability(IndexedGeometryArray.ALLOW_NORMAL_INDEX_WRITE)

      val s3d = new Shape3D(ta)

      s3d.setPickable(true)
      s3d.setUserData(subset.e)

      s3d.setCapability(Shape3D.ALLOW_GEOMETRY_READ)
      s3d.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE)

      s3d.setCapability(Shape3D.ALLOW_APPEARANCE_READ)
      s3d.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE)
      s3d
    })

    //		val ret = new IndexedTriangleArray(verts.size, GeometryArray.COORDINATES | GeometryArray.COLOR_4 | GeometryArray.NORMALS, verts.size)
    //		ret.setCapability(GeometryArray.ALLOW_COORDINATE_READ)
    //		ret.setCapability(GeometryArray.ALLOW_COLOR_READ)
    //		ret.setCapability(GeometryArray.ALLOW_COLOR_WRITE)
    //		ret.setCapability(IndexedGeometryArray.ALLOW_COLOR_INDEX_READ)
    //		ret.setCapability(IndexedGeometryArray.ALLOW_COLOR_INDEX_WRITE)
    //		ret.setCapability(IndexedGeometryArray.ALLOW_NORMAL_INDEX_READ)
    //		ret.setCapability(IndexedGeometryArray.ALLOW_NORMAL_INDEX_WRITE)
    //		ret.setCapability(GeometryArray.ALLOW_NORMAL_READ)
    //		ret.setCapability(GeometryArray.ALLOW_NORMAL_WRITE)
    //		
    //		
    //		ret.setCoordinates(0, verts.toArray)
    //		ret.setCoordinateIndices(0, verts.indices.toArray)

    return (allComponents.toArray, sphereBranchGroup, ita, terrainVertToContourTreeNode.toArray)
  }

  def triangulateTerminalPolygon(Pinit: Array[Point2D], quadVertices: ArrayBuffer[Point3f], zOuter: Double, zInner: Double,
    sphereBranchGroup: BranchGroup, noncriticalNodes: List[ContourTreeNode], getVertexHeight: Int => Double, terrainVertToContourTreeNode: ArrayBuffer[Int],
    outerNode: Int, innerNode: Int): Int = {
    //		val scaleFactors = Array(.95, .90, .80, .70, .55, .20, .05)
    //		val alphaVals = Array(.95, .85, .65, .4, .2, .1, .03)
    val scaleFactors = Array(.5)
    val alphaVals = Array(.5)
    //		val scaleFactors = Array.empty[Double]
    //		val alphaVals = Array.empty[Double]

    val numLayers = scaleFactors.size

    var alphaOut = 1d
    var P = Pinit
    var totalVertsAdded = 0
    scaleFactors.zip(alphaVals).foreach {
      case (scaleFactor, alphaIn) => {
        val Q = scaleContour(P, scaleFactor)

        val zOut = alphaOut * zOuter + (1 - alphaOut) * zInner
        val zIn = alphaIn * zOuter + (1 - alphaIn) * zInner

        val zMin = math.min(zOut, zIn)
        val zMax = math.max(zOut, zIn)

        val ncStratum = noncriticalNodes.flatMap(n => {
          val z = getVertexHeight(n.vertex)
          if (z >= zMin && z < zMax)
            Some(n)
          else
            None
        })

        val nOuter = if (scaleFactor == .95) outerNode else -1

        totalVertsAdded += triangulateAnnulus(P, Q, quadVertices, zOut, zIn, sphereBranchGroup, ncStratum, getVertexHeight, terrainVertToContourTreeNode, nOuter, -1)
        alphaOut = alphaIn
        P = Q
      }
    }

    def makePoint3f(xy: Point2D, d3: Double) = new Point3f(xy.getX.toFloat - .5f, xy.getY.toFloat - .5f, d3.toFloat)

    val center = PolygonUtilities.centerOfMass(P)
    if (center.getX.isInfinity || center.getY.isInfinity) {
      val xSum = P.map(_.getX).sum
      val ySum = P.map(_.getY).sum
      center.setLocation(xSum / P.size.toDouble, ySum / P.size.toDouble)
    }
    val zOut = alphaOut * zOuter + (1 - alphaOut) * zInner
    (0 until P.size).foreach(i => {
      val j = (i + 1) % P.size
      val p1 = makePoint3f(P(i), zOut)
      val p2 = makePoint3f(P(j), zOut)
      val p3 = makePoint3f(center, zInner)
      quadVertices ++= List(p1, p2, p3)
      terrainVertToContourTreeNode ++= List(-1, -1, innerNode)
      totalVertsAdded += 3
    })

    // Stuff for computing the spheres
    val zMin = math.min(zOut, zInner)
    val zMax = math.max(zOut, zInner)

    val ncStratum = noncriticalNodes.flatMap(n => {
      val z = getVertexHeight(n.vertex)
      if (z >= zMin && z < zMax)
        Some(n)
      else
        None
    })

    return totalVertsAdded
    //		Point2D center = PolygonUtilities.centerOfMass(node.poly);
    //		double funcOut = alphaOut * outerFuncVal + (1 - alphaOut) * innerFuncVal;
    //		double zOut = functionValToHeight((float)funcOut, mmr);
    //		double zIn = functionValToHeight((float)innerFuncVal, mmr);
    //		Color3f outerColor = new Color3f(Colormaps.getColor((float)funcOut, mmr[0], mmr[2], cmap));
    //		Color3f innerColor = new Color3f(Colormaps.getColor((float)innerFuncVal, mmr[0], mmr[2], cmap));
    //		for (int i = 0; i < P.length; i++) {
    //			int j;
    //			if (i == P.length - 1)
    //				j = 0;
    //			else
    //				j = i+1;
    //			
    //			Point3f p1 = new Point3f((float)P[i].getX(), (float)P[i].getY(), (float)zOut);
    //			Point3f p2 = new Point3f((float)P[j].getX(), (float)P[j].getY(), (float)zOut);
    //			Point3f p3 = new Point3f((float)center.getX(), (float)center.getY(), (float)zIn);
    //			
    //			// Counter-clockwise
    //			quadVertices.add((Point3f)p2.clone());
    //			quadVertices.add((Point3f)p1.clone());
    //			quadVertices.add((Point3f)p3.clone());
    //			colors.add(outerColor);
    //			colors.add(outerColor);
    //			colors.add(innerColor);
    //		}

  }
  //		Point2D [] P = node.poly;
  //		double alphaOut = 1f;
  //		for (int layerNum = 0; layerNum < numLayers; layerNum++) {
  //			double scaleFactor = scaleFactors[layerNum];
  //			Point2D [] Q = node.scalePoly(node.poly, scaleFactor);
  //			
  //			double alphaIn = alphaVals[layerNum];
  //			
  //			double funcOut = alphaOut * outerFuncVal + (1 - alphaOut) * innerFuncVal;
  //			double funcIn = alphaIn * outerFuncVal + (1 - alphaIn) * innerFuncVal;
  //			
  //			double zOut = functionValToHeight((float)funcOut, mmr);
  //			double zIn = functionValToHeight((float)funcIn, mmr);
  //			
  //			Color3f outerColor = new Color3f(Colormaps.getColor((float)funcOut, mmr[0], mmr[2], cmap));
  //			Color3f innerColor = new Color3f(Colormaps.getColor((float)funcIn, mmr[0], mmr[2], cmap));
  //			
  //			for (int i = 0; i < P.length; i++) {
  //				int j;
  //				if (i == P.length - 1)
  //					j = 0;
  //				else
  //					j = i+1;
  //				Point3f ul = new Point3f((float)P[i].getX(), (float)P[i].getY(), (float)zOut);
  //				Point3f ur = new Point3f((float)P[j].getX(), (float)P[j].getY(), (float)zOut);
  //				Point3f ll = new Point3f((float)Q[i].getX(), (float)Q[i].getY(), (float)zIn);
  //				Point3f lr = new Point3f((float)Q[j].getX(), (float)Q[j].getY(), (float)zIn);
  //				
  //				// Counter-clockwise
  //				quadVertices.add((Point3f)ur.clone());
  //				quadVertices.add((Point3f)ul.clone());
  //				quadVertices.add((Point3f)ll.clone());
  //				colors.add(outerColor);
  //				colors.add(outerColor);
  //				colors.add(innerColor);
  //				
  //				quadVertices.add((Point3f)ll.clone());
  //				quadVertices.add((Point3f)lr.clone());
  //				quadVertices.add((Point3f)ur.clone());
  //				colors.add(innerColor);
  //				colors.add(innerColor);
  //				colors.add(outerColor);
  //			}
  //			alphaOut = alphaIn;
  //			P = Q;
  //		}
  //		
  //		Point2D center = PolygonUtilities.centerOfMass(node.poly);
  //		double funcOut = alphaOut * outerFuncVal + (1 - alphaOut) * innerFuncVal;
  //		double zOut = functionValToHeight((float)funcOut, mmr);
  //		double zIn = functionValToHeight((float)innerFuncVal, mmr);
  //		Color3f outerColor = new Color3f(Colormaps.getColor((float)funcOut, mmr[0], mmr[2], cmap));
  //		Color3f innerColor = new Color3f(Colormaps.getColor((float)innerFuncVal, mmr[0], mmr[2], cmap));
  //		for (int i = 0; i < P.length; i++) {
  //			int j;
  //			if (i == P.length - 1)
  //				j = 0;
  //			else
  //				j = i+1;
  //			
  //			Point3f p1 = new Point3f((float)P[i].getX(), (float)P[i].getY(), (float)zOut);
  //			Point3f p2 = new Point3f((float)P[j].getX(), (float)P[j].getY(), (float)zOut);
  //			Point3f p3 = new Point3f((float)center.getX(), (float)center.getY(), (float)zIn);
  //			
  //			// Counter-clockwise
  //			quadVertices.add((Point3f)p2.clone());
  //			quadVertices.add((Point3f)p1.clone());
  //			quadVertices.add((Point3f)p3.clone());
  //			colors.add(outerColor);
  //			colors.add(outerColor);
  //			colors.add(innerColor);
  //		}
  //	}

  def triangulateAnnulus(P: Array[Point2D], Q: Array[Point2D], quadVertices: ArrayBuffer[Point3f], zOuter: Double, zInner: Double,
    sphereBranchGroup: BranchGroup, noncriticalNodes: List[ContourTreeNode], getVertexHeight: Int => Double, terrainVertToContourTreeNode: ArrayBuffer[Int],
    outerNode: Int, innerNode: Int): Int = {
    assert(P.size == Q.size)

    def makePoint3f(xy: Point2D, d3: Double) = new Point3f(xy.getX.toFloat - .5f, xy.getY.toFloat - .5f, d3.toFloat)

    var totalVertsAdded = 0

    (0 until P.size).foreach(i => {
      val j = (i + 1) % P.size

      val ul = makePoint3f(P(i), zOuter)
      val ur = makePoint3f(P(j), zOuter)
      val ll = makePoint3f(Q(i), zInner)
      val lr = makePoint3f(Q(j), zInner)

      // Counter-clockwise
      quadVertices ++= List(ur, ul, ll)
      terrainVertToContourTreeNode ++= List(outerNode, outerNode, innerNode)

      quadVertices ++= List(ll, lr, ur)
      terrainVertToContourTreeNode ++= List(innerNode, innerNode, outerNode)

      totalVertsAdded += 6
    })

    //		noncriticalNodes.foreach(n => {
    //			val i = (P.size * math.random).toInt
    //			val j = (i+1) % P.size
    //			val z = getVertexHeight(n.vertex)
    //			val alpha = if (zOuter == zInner) 1.0 else (z - zOuter) / (zInner - zOuter)
    //			val x1 = P(i).getX * (1-alpha) + Q(i).getX * alpha
    //			val y1 = P(i).getY * (1-alpha) + Q(i).getY * alpha
    //			
    //			val x2 = P(j).getX * (1-alpha) + Q(j).getX * alpha
    //			val y2 = P(j).getY * (1-alpha) + Q(j).getY * alpha
    //			
    //			val alpha2 = math.random
    //			
    //			val x = x1 * (1-alpha2) + x2 * alpha2
    //			val y = y1 * (1-alpha2) + y2 * alpha2
    //			
    //			val t = new Transform3D()
    //			t.setTranslation(new Vector3f(x.toFloat - .5f, y.toFloat - .5f, z.toFloat))
    //			val tg = new TransformGroup(t)
    //			
    //			val appearance = new Appearance()
    //			appearance.setCapability(Appearance.ALLOW_MATERIAL_READ)
    //			appearance.setCapability(Appearance.ALLOW_MATERIAL_WRITE)
    //			val colorAtts = new ColoringAttributes()
    //			colorAtts.setShadeModel(ColoringAttributes.NICEST)
    //			
    //			val material = new Material()
    //			material.setCapability(Material.ALLOW_COMPONENT_READ)
    //			material.setCapability(Material.ALLOW_COMPONENT_WRITE)
    //			material.setDiffuseColor(1f, 0f, 0f)
    //			material.setSpecularColor(1f, 1f, 1f)
    //			material.setShininess(115f)
    //			appearance.setMaterial(material)
    //			
    //			val sphere = new Sphere(.001f)
    //			sphere.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_READ)
    //			sphere.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_WRITE)
    //			sphere.setCapability(Primitive.ENABLE_APPEARANCE_MODIFY)
    //			sphere.setCapability(Shape3D.ALLOW_APPEARANCE_READ)
    //			sphere.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE)
    //			sphere.getShape().setAppearance(appearance)
    //			sphere.setAppearance(appearance)
    //			sphere.setPickable(true)
    //			sphere.getShape().setUserData(n)
    //			
    ////			val transAtts = new TransparencyAttributes(TransparencyAttributes.NICEST, .5f)
    ////			sphere.getAppearance().setTransparencyAttributes(transAtts)
    //			
    //			tg.addChild(sphere);
    //			sphereBranchGroup.addChild(tg);
    //		})

    return totalVertsAdded
  }

  def scaleContour(contour: Array[Point2D], scaleFactor: Double): Array[Point2D] = {
    if (contour.isEmpty)
      return Array.empty[Point2D]
    val oldCentroid = PolygonUtilities.centerOfMass(contour)
    val contour2 = new Array[Point2D](contour.size)
    AffineTransform.getScaleInstance(scaleFactor, scaleFactor).transform(contour, 0, contour2, 0, contour.size)
    val newCentroid = PolygonUtilities.centerOfMass(contour2)

    val tx = oldCentroid.getX - newCentroid.getX
    val ty = oldCentroid.getY - newCentroid.getY
    AffineTransform.getTranslateInstance(tx, ty).transform(contour2, 0, contour2, 0, contour2.size)

    return contour2
  }

  def splitContourCirclePartition(bdry: Array[Point2D], alphas: Seq[Double], rule: SplitRule): Seq[Array[Point2D]] = {

    if (bdry.isEmpty)
      return Seq.fill(alphas.size)(Array.empty[Point2D])

    // Find the farthest pair of points
    class DistTriple(val i: Int, val j: Int, val dSq: Double) extends Ordered[DistTriple] {
      def compare(that: DistTriple): Int = dSq.compareTo(that.dSq)
    }
    val farthestPair = (0 until bdry.size).flatMap(i => {
      (i + 1 until bdry.size).map(j => {
        new DistTriple(i, j, bdry(i).distanceSq(bdry(j)))
      })
    }).max

    // Build vector from one pole to the other, and compute projections of all points onto this vector.
    val polePt1 = bdry(farthestPair.i)
    val polePt2 = bdry(farthestPair.j)
    val vPole = Array[Double](polePt2.getX - polePt1.getX,
      polePt2.getY - polePt1.getY)
    val vPoleNorm = math.sqrt(vPole(0) * vPole(0) + vPole(1) * vPole(1))
    vPole.indices.foreach(vPole(_) /= vPoleNorm)
    val vTheta = math.atan2(vPole(1), vPole(0))

    val trans = new AffineTransform
    trans.rotate(-vTheta)
    trans.translate(-polePt1.getX, -polePt1.getY)
    // Create a rotated version of the points
    val bdryRotated = bdry.map(p =>
      trans.transform(p, null) //			new Point2D.Double((p.getX - polePt1.getX)*math.cos(-vTheta), (p.getY - polePt1.getY)*math.sin(-vTheta))
      ).sortWith((p1, p2) => p1.getX < p2.getX)

    // Get each point onto the positive y-axis, adding in its opposite point
    val ptsYAxis = bdryRotated.zipWithIndex.map {
      case (p, i) =>
        if (i == 0) {
          p
        } else if (i == bdryRotated.size - 1) {
          p
        } else {
          val s = math.signum(p.getY)
          val pLeft = (i - 1 to 0 by -1).view.map(bdryRotated(_)).find(p => math.signum(p.getY) != s) match {
            case Some(point) => point
            case None => bdryRotated(0)
          }
          val pRight = (i + 1 until bdryRotated.size).view.map(bdryRotated(_)).find(p => math.signum(p.getY) != s) match {
            case Some(point) => point
            case None => bdryRotated(bdryRotated.size - 1)
          }

          // Calculate the new y-value
          val alpha = if (pRight.getX != pLeft.getX) (p.getX - pLeft.getX) / (pRight.getX - pLeft.getX) else 0d
          val yAdd = pLeft.getY + alpha * (pRight.getY - pLeft.getY)
          val yNew = math.abs(p.getY) + math.abs(yAdd)
          new Point2D.Double(p.getX, yNew)
        }
    }

    val trueArea = PolygonUtilities.getArea(bdry)
    if (trueArea.isNaN) {
      //			System.err.println("Warning:  Zero-area polygon")
      //			return Seq.empty[Array[Point2D]]
      return Seq.fill(alphas.size)(Array.empty[Point2D])
    }

    // NEW CODE
    //		println("Splitting contour into " + alphas.size + " parts.")

    var slicedBdry = bdry

    val subContours = alphas.dropRight(1).map(alpha => {
      val targetArea = trueArea * (alpha)
      //			println("Alpha is " + alpha)
      // now find the split point
      var areaSum = 0d
      var xSplit = -1d
      (0 until bdry.size - 1).foreach(i => {
        val p1 = ptsYAxis(i)
        val p2 = ptsYAxis(i + 1)
        val dx = p2.getX - p1.getX
        val areaRight = areaSum + dx * (p2.getY + p1.getY) / 2.0
        if (targetArea >= areaSum && targetArea <= areaRight) {
          // Calculate the split point here
          val s = targetArea - areaSum
          val a = p1.getY
          val b = p2.getY
          val h = p2.getX - p1.getX
          def quadForm(a: Double, b: Double, c: Double) = (-b + math.sqrt(b * b - 4 * a * c)) / (2.0 * a)

          val xDelta = if (a == b) 0 else quadForm((b - a) / (2 * h), a, -s)
          xSplit = p1.getX + xDelta
          if (xSplit.isNaN) {
            val t = 3
          }
          val areaDebug = areaSum + xDelta * (p2.getY + p1.getY) / 2.0
          //					println("Desired area:  " + targetArea + "\tDebug area:  " + areaDebug)
        }
        areaSum = areaRight
      })

      assert(areaSum >= 0)

      val split1Pre = new Point2D.Double(xSplit, -10)
      val split2Pre = new Point2D.Double(xSplit, 10)
      val split1Post = trans.inverseTransform(split1Pre, null)
      val split2Post = trans.inverseTransform(split2Pre, null)

      splitPolygonAtLine(split1Post, split2Post, slicedBdry) match {
        case Some((c1, c2)) => {
          val area1 = PolygonUtilities.getArea(c1)
          val area2 = PolygonUtilities.getArea(c2)

          if (area1.isNaN || area2.isNaN) {
            val t = 3
          }
          //					println("\tPoly areas:  " + area1 + "\t" + area2)
          val frac1 = area1 / trueArea
          val frac2 = area2 / trueArea
          val alphaBdry = 1 - alpha
          //					println(List(frac1, frac2, alphaBdry).mkString(","))
          if (math.abs(alphaBdry - frac1) < math.abs(alphaBdry - frac2)) {
            slicedBdry = c1
            //						println("A")
            c2
          } else {
            //						println("B")
            slicedBdry = c2
            c1
          }
        }
        case None => {
          System.err.println("Warning:  Impossible split")
          Array.empty[Point2D]
        }
      }
    })
    if (slicedBdry.flatMap(p => List(p.getX, p.getY)).exists(_.isNaN)) {
      assert(false)
    }

    subContours.foreach(c => if (c.flatMap(p => List(p.getX, p.getY)).exists(_.isNaN)) {
      assert(false)
    })

    if (alphas.size > 2) {
      val z = 3
    }

    return subContours.toSeq ++ List(slicedBdry)
    // END NEW CODE
    /*		
		// Sanity check:  Add up the area and see what we get
		val trueArea = PolygonUtilities.getArea(bdry)
		val targetArea = trueArea * alpha
		val checkArea = (0 until bdry.size-1).map(i => {
			val p1 = ptsYAxis(i)
			val p2 = ptsYAxis(i+1)
			val dx = p2.getX-p1.getX
			dx * (p2.getY + p1.getY) / 2.0
		}).sum
		
		//println("True area:  " + trueArea + "\tIntegrated area:  " + checkArea)
		
		// now find the split point
		var areaSum = 0d
		var xSplit = -1d 
		(0 until bdry.size-1).foreach(i => {
			val p1 = ptsYAxis(i)
			val p2 = ptsYAxis(i+1)
			val dx = p2.getX-p1.getX
			val areaRight = areaSum + dx*(p2.getY + p1.getY) / 2.0
			if (targetArea >= areaSum && targetArea <= areaRight) {
				// Calculate the split point here
				val s = targetArea - areaSum
				val a = p1.getY
				val b = p2.getY
				val h = p2.getX - p1.getX
				def quadForm(a: Double, b: Double, c: Double) = (-b + math.sqrt(b*b - 4*a*c)) / (2.0*a)
				
				val xDelta = quadForm((b-a)/(2*h), a, -s)
//				val xDelta = (-2*a + math.sqrt(4*a*a + 8*m*s)) / (2*m)
				xSplit = p1.getX + xDelta
				val areaDebug = areaSum + xDelta * (p2.getY + p1.getY) / 2.0
				//println("Desired area:  " + targetArea + "\tDebug area:  " + areaDebug)
//				val s = targetArea - areaSum
//				//xSplit = (p2.getX - p1.getX) * (s / areaRight) + p1.getX
//				val b1 = p1.getY
//				val b2 = p2.getY
//				val h = p2.getX - p1.getX
//				val c = b1 / (b2 * h)
//				val xDelta = (-b2 - math.sqrt(b2*b2 + 8*c*b2*s)) / (2*c*b2)
//				xSplit = p1.getX + xDelta
//				val areaDebug = areaSum + xDelta * (p2.getY + p1.getY) / 2.0
//				println("Desired area:  " + targetArea + "\tDebug area:  " + areaDebug)
			}
			areaSum = areaRight
		})
		
		assert(areaSum >= 0)
		
//		def splitPolygonAtLine(q: Point2D, v: Array[Double], bdry: Array[Point2D]): Option[(Array[Point2D], Array[Point2D])]
		
		val split1Pre = new Point2D.Double(xSplit, -10)
		val split2Pre = new Point2D.Double(xSplit, 10)
		val split1Post = trans.inverseTransform(split1Pre, null)
		val split2Post = trans.inverseTransform(split2Pre, null)
//		
//		val w = Array[Double](split2Post.getX - split1Post.getX, split2Post.getY - split1Post.getY)
//		val wNorm = math.sqrt(w(0)*w(0) + w(1)*w(1))
//		w.indices.foreach(w(_) /= wNorm)
		
//		val qPre = new Point2D.Double(xSplit, 0)
//		val qPost = trans.inverseTransform(qPre, null)
		
		splitPolygonAtLine(split1Post, split2Post, bdry) match {
			case Some((c1, c2)) => {
				val area1 = PolygonUtilities.getArea(c1)
				val area2 = PolygonUtilities.getArea(c2)
				//println("\tPoly areas:  " + area1 + "\t" + area2)
				val frac1 = area1 / trueArea
				val frac2 = area2 / trueArea
				if (math.abs(alpha - frac1) < math.abs(alpha - frac2)) {
					return (c1, c2)
				} else {
					return (c2, c1)
				}
			}
			case None => {throw new RuntimeException}
		}
		throw new RuntimeException("What went wrong?")
		return null
		*/

    //		var areaSum = 0d
    //		(0 until bdryRotated.size - 1).foreach{i =>
    //			
    //		}

    //		class Projection(val i: Int) extends Ordered[Projection] {
    //			val proj = {
    //				val w = new Point2D.Double(bdry(i).getX - bdry(farthestPair.i).getX, bdry(i).getY - bdry(farthestPair.i).getY)
    //				w.x * vPole(0) + w.y * vPole(1)
    //			}
    //			def compare(that: Projection): Int = this.proj.compareTo(that.proj)
    //		}
    //		
    //		val projections = bdry.indices.map(new Projection(_)).sorted

    /*		
		val pointProjectionPairs = bdry.indices.map(i => {
			val w = new Point2D.Double(bdry(i).getX - bdry(farthestPair.i).getX, bdry(i).getY - bdry(farthestPair.i).getY)
			(i, w.x * vPole(0) + w.y * vPole(1))
		}).sortWith((t1, t2) => t1._2 < t2._2).toArray
		
//		val oppositePoints = pointProjectionPairs.foreach()
		
		// Inverted index
		val pointToProjIdx = new Array[Int](bdry.size)
		pointProjectionPairs.indices.foreach(i => pointToProjIdx(pointProjectionPairs(i)._1) = i)
		
		val uOut = new Array[Double](2)
		val pointToOpposite = new scala.collection.mutable.HashMap[Int, Point2D]
		bdry.indices.foreach(i => {
			val j = (i + 1) % bdry.size
			val idx1 = math.min(pointToProjIdx(i), pointToProjIdx(j))
			val idx2 = math.max(pointToProjIdx(i), pointToProjIdx(j))
			(idx1 + 1 until idx2).map(pointProjectionPairs(_)._1).foreach(k => {
				// k'th boundary vertex splits polygon edge (i, j)
				// Construct vector (w) from k'th boundary point towards the polygon diameter
				val kProj = pointProjectionPairs(pointToProjIdx(k))._2
				val kOnDiameter = new Point2D.Double(bdry(farthestPair.i).getX + kProj * v(0), bdry(farthestPair.i).getY + kProj * v(1))
				val w = Array[Double](kOnDiameter.getX - bdry(k).getX, kOnDiameter.getY - bdry(k).getY)
				val wNorm = math.sqrt(w(0)*w(0) + w(1)*w(1))
				w.indices.foreach(w(_) /= wNorm)
				val kFarAway = new Point2D.Double(bdry(k).getX + 50*w(0), bdry(k).getY + 50*w(1))
				// Now compute the intersection of vector w with polygon edge (i, j)
				getIntersectU(bdry(i), bdry(j), bdry(k), kFarAway, uOut) match {
					case Some(kOpposite) => {
						pointToOpposite(k) = kOpposite
					}
					case None => {
//						saveImage("oops.png")
//						throw new RuntimeException()
					}
				}
			})
		})
		
		assert(pointToOpposite.size == bdry.size - 2)
		val bdryArea = PolygonUtilities.getArea(bdry)
		
		// Determine where to slice the polygon
		val targetArea = alpha * bdryArea
		var areaSum = 0d
		pointProjectionPairs.indices.dropRight(1).foreach{i => {
			val j = i + 1
			val h = pointProjectionPairs(j)._2 - pointProjectionPairs(i)._2
			if (i == 0) {
				// Triangle
				val pj = bdry(pointProjectionPairs(j)._1)
				val pjOpp = pointToOpposite(pointProjectionPairs(j)._1)
				val b = pj.distance(pjOpp)
				val subArea = .5 * b * h
				if (targetArea <= areaSum + subArea) {
					val hTarget = 2*targetArea / b
					val q = new Point2D.Double(bdry(farthestPair.i).getX + v(0)*hTarget,
																			bdry(farthestPair.i).getY + v(1)*hTarget)
					val vPerp = Array[Double](-v(1), v(0))
					splitPolygonAtLine(q, vPerp, bdry) match {
						case Some((c1, c2)) => {
							val a1 = PolygonUtilities.getArea(c1)
							val a2 = PolygonUtilities.getArea(c2)
							if (math.abs(a1 - targetArea) < math.abs(a2 - targetArea)) {
								g2d.setColor(new Color(0, 255, 0, 127))
								val drawPts1 = c1.map(p => scale(p.getX, p.getY))
								g2d.fill(new Polygon(drawPts1.map(_.x), drawPts1.map(_.y), drawPts1.size))
								
								g2d.setColor(new Color(255, 0, 0, 127))
								val drawPts2 = c2.map(p => scale(p.getX, p.getY))
								g2d.fill(new Polygon(drawPts2.map(_.x), drawPts2.map(_.y), drawPts2.size))
								saveImage
								return (c1, c2)
							} else {
								g2d.setColor(new Color(0, 255, 0, 127))
								val drawPts1 = c1.map(p => scale(p.getX, p.getY))
								g2d.fill(new Polygon(drawPts1.map(_.x), drawPts1.map(_.y), drawPts1.size))
								
								g2d.setColor(new Color(255, 0, 0, 127))
								val drawPts2 = c2.map(p => scale(p.getX, p.getY))
								g2d.fill(new Polygon(drawPts2.map(_.x), drawPts2.map(_.y), drawPts2.size))
								saveImage
								return (c2, c1)
							}
						}
						case None => {
							saveImage
							return null
						}
					}
					
				} else {
					areaSum += subArea
				}
			} else if (j == bdry.size-1) {
				// Triangle
				val pi = bdry(pointProjectionPairs(i)._1)
				val piOpp = pointToOpposite(pointProjectionPairs(i)._1)
				val b = pi.distance(piOpp)
				val subArea = .5 * b * h
				assert(targetArea <= areaSum + subArea)
				val hTarget = 2*(bdryArea - targetArea) / b
				val q = new Point2D.Double(bdry(farthestPair.j).getX - v(0)*hTarget,
																		bdry(farthestPair.j).getY - v(1)*hTarget)
				val vPerp = Array[Double](-v(1), v(0))
				splitPolygonAtLine(q, vPerp, bdry) match {
						case Some((c1, c2)) => {
							val a1 = PolygonUtilities.getArea(c1)
							val a2 = PolygonUtilities.getArea(c2)
							if (math.abs(a1 - targetArea) < math.abs(a2 - targetArea)) {
								g2d.setColor(new Color(0, 255, 0, 127))
								val drawPts1 = c1.map(p => scale(p.getX, p.getY))
								g2d.fill(new Polygon(drawPts1.map(_.x), drawPts1.map(_.y), drawPts1.size))
								
								g2d.setColor(new Color(255, 0, 0, 127))
								val drawPts2 = c2.map(p => scale(p.getX, p.getY))
								g2d.fill(new Polygon(drawPts2.map(_.x), drawPts2.map(_.y), drawPts2.size))
								saveImage
								return (c1, c2)
							} else {
								g2d.setColor(new Color(0, 255, 0, 127))
								val drawPts1 = c1.map(p => scale(p.getX, p.getY))
								g2d.fill(new Polygon(drawPts1.map(_.x), drawPts1.map(_.y), drawPts1.size))
								
								g2d.setColor(new Color(255, 0, 0, 127))
								val drawPts2 = c2.map(p => scale(p.getX, p.getY))
								g2d.fill(new Polygon(drawPts2.map(_.x), drawPts2.map(_.y), drawPts2.size))
								saveImage
								return (c2, c1)
							}
						}
						case None => {
							saveImage
							return null
						}
					}
			} else {
				// Trapezoid
				val pi = bdry(pointProjectionPairs(i)._1)
				val piOpp = pointToOpposite(pointProjectionPairs(i)._1)
				val b1 = pi.distance(piOpp)
				
				val pj = bdry(pointProjectionPairs(j)._1)
				val pjOpp = pointToOpposite(pointProjectionPairs(j)._1)
				val b2 = pj.distance(pjOpp)
				val subArea = .5 * (b1 + b2) * h
				if (targetArea <= areaSum + subArea) {
					val c = b1 / (b2 * h)
					val hTarget = pointProjectionPairs(i)._2 + (-b2 + math.sqrt(b2*b2 + 8*c*b2*targetArea)) / (2*c*b2)
					val q = new Point2D.Double(bdry(farthestPair.i).getX + v(0)*hTarget,
																			bdry(farthestPair.i).getY + v(1)*hTarget)
					val vPerp = Array[Double](-v(1), v(0))
					splitPolygonAtLine(q, vPerp, bdry) match {
						case Some((c1, c2)) => {
							val a1 = PolygonUtilities.getArea(c1)
							val a2 = PolygonUtilities.getArea(c2)
							if (math.abs(a1 - targetArea) < math.abs(a2 - targetArea)) {
								g2d.setColor(new Color(0, 255, 0, 127))
								val drawPts1 = c1.map(p => scale(p.getX, p.getY))
								g2d.fill(new Polygon(drawPts1.map(_.x), drawPts1.map(_.y), drawPts1.size))
								
								g2d.setColor(new Color(255, 0, 0, 127))
								val drawPts2 = c2.map(p => scale(p.getX, p.getY))
								g2d.fill(new Polygon(drawPts2.map(_.x), drawPts2.map(_.y), drawPts2.size))
								saveImage
								return (c1, c2)
							} else {
								g2d.setColor(new Color(0, 255, 0, 127))
								val drawPts1 = c1.map(p => scale(p.getX, p.getY))
								g2d.fill(new Polygon(drawPts1.map(_.x), drawPts1.map(_.y), drawPts1.size))
								
								g2d.setColor(new Color(255, 0, 0, 127))
								val drawPts2 = c2.map(p => scale(p.getX, p.getY))
								g2d.fill(new Polygon(drawPts2.map(_.x), drawPts2.map(_.y), drawPts2.size))
								saveImage
								return (c2, c1)
							}
						}
						case None => {
							saveImage
							return null
						}
					}
				
				} else {
					areaSum += subArea
				}
			}
		}}
		
//		println("bdry area:  " + bdryArea + "\tCalced:  " + sumArea)
		
		throw new RuntimeException("Unable to split polygon")
		*/
  }

  def splitContour(bdry: Array[Point2D], alpha: Double, rule: SplitRule, epsilon: Double, delta: Double, pctChange: Double): (Array[Point2D], Array[Point2D]) = {
    val (pct1, pct2, swap) = if (alpha < .5) {
      (alpha, 1 - alpha, false)
    } else {
      (1 - alpha, alpha, true)
    }

    val gp = getGeneralPath(bdry)
    val brect = gp.getBounds2D

    val bdryArea = PolygonUtilities.getArea(bdry)

    var p1 = getRandomPointInPolygon(gp, brect)
    var p2 = getRandomPointInPolygon(gp, brect)

    var w1 = .5d
    var w2 = .5d

    Stream.from(0).foreach(_ => {
      val sumWeight = w1 + w2
      val d = p1.distance(p2)
      if (sumWeight > d) {
        w1 = d * w1 / sumWeight
        w2 = d * w2 / sumWeight
      }

      var stable = true
      getVoronoiPolygons(bdry, p1, p2, w1, w2) match {
        case Some((v1, v2)) => {
          val areaCell1 = PolygonUtilities.getArea(v1)
          val areaCell2 = PolygonUtilities.getArea(v2)
          val pctAreaCell1 = areaCell1 / bdryArea
          val pctAreaCell2 = areaCell2 / bdryArea

          if (math.abs(pctAreaCell1 - pct1) > epsilon)
            stable = false

          // Adjust weights
          w1 += w1 * (pct1 - pctAreaCell1) / pct1 * pctChange
          w1 = math.max(w1, delta)

          w2 += w2 * (pct2 - pctAreaCell2) / pct2 * pctChange
          w2 = math.max(w2, delta)

          // Move generators
          val center1 = PolygonUtilities.centerOfMass(v1)
          val center2 = PolygonUtilities.centerOfMass(v2)

          var success = true
          success &= moveGenerator(gp, center1, p1, bdry)
          success &= moveGenerator(gp, center2, p2, bdry)

          if (!success) {
            println("moveGenerator failed.")
            p1 = getRandomPointInPolygon(gp, brect)
            p2 = getRandomPointInPolygon(gp, brect)
            val d = p1.distance(p2)
            val randPct1 = math.random
            val randPct2 = 1 - randPct1
            w1 = randPct1 * d
            w2 = randPct2 * d
            println("Resetting weights and generator locations...")
            stable = false
            // continue
          }
          if (stable) {
            println("Converged:  " + System.currentTimeMillis)
            if (swap) {
              return (v2, v1)
            } else {
              return (v1, v2)
            }
          }
        }
        case None => {
          p1 = getRandomPointInPolygon(gp, brect)
          p2 = getRandomPointInPolygon(gp, brect)
          val d = p1.distance(p2)
          val randPct1 = math.random
          val randPct2 = 1 - randPct1
          w1 = randPct1 * d
          w2 = randPct2 * d
          println("Resetting weights and generator locations...")
          stable = false
          // continue
        }
      }
    })

    null
  }

  def moveGenerator(gp: GeneralPath, center: Point2D, p: Point2D, bdry: Array[Point2D]): Boolean = {
    if (gp.contains(center)) {
      p.setLocation(center.getX, center.getY)
      return true
    } else {
      var nearestU = Double.MaxValue
      var nearestPt: Point2D = null
      val uOut = new Array[Double](2)
      (0 until bdry.length).foreach(i => {
        val j = (i + 1) % bdry.length
        val b1 = bdry(i)
        val b2 = bdry(j)
        getIntersectU(p, center, b1, b2, uOut) match {
          case Some(isect) => {
            if (uOut(0) < nearestU) {
              nearestU = uOut(0)
              nearestPt = isect
            }
          }
          case None => { /*Do nothing*/ }
        }
      })
      if (nearestPt == null)
        return false
      p.setLocation(nearestPt.getX, nearestPt.getY)
      return true
    }
  }
  def splitPolygonAtLine(lineEnd1: Point2D, lineEnd2: Point2D, bdry: Array[Point2D]): Option[(Array[Point2D], Array[Point2D])] = {
    var isect1: Point2D = null
    var idxIsect1: Int = -1
    var isect2: Point2D = null
    var idxIsect2: Int = -1
    val uOut = new Array[Double](2)
    (0 until bdry.length).foreach(i => {
      val j = (i + 1) % bdry.length
      getIntersectU(lineEnd1, lineEnd2, bdry(i), bdry(j), uOut) match {
        case Some(isect) => {
          if (isect1 == null) {
            isect1 = isect
            idxIsect1 = i
          } else {
            isect2 = isect
            idxIsect2 = i
          }
        }
        case None => { /*Do nothing*/ }
      }
    })

    if (isect1 == null || isect2 == null)
      return None

    val halfBdry1 = new ArrayBuffer[Point2D]
    val halfBdry2 = new ArrayBuffer[Point2D]
    var activeHalfBdry = halfBdry1

    (0 until bdry.size).foreach(i => {
      activeHalfBdry += bdry(i).clone.asInstanceOf[Point2D]
      if (i == idxIsect1 && activeHalfBdry == halfBdry1) {
        halfBdry1 += isect1
        halfBdry1 += isect2
        activeHalfBdry = halfBdry2
      } else if (i == idxIsect2 && activeHalfBdry == halfBdry2) {
        halfBdry2 += isect2
        halfBdry2 += isect1
        activeHalfBdry = halfBdry1
      } else if (i == idxIsect1 && activeHalfBdry == halfBdry2) {
        halfBdry2 += isect1
        halfBdry2 += isect2
        activeHalfBdry = halfBdry1
      } else if (i == idxIsect2 && activeHalfBdry == halfBdry1) {
        halfBdry1 += isect2
        halfBdry1 += isect1
        activeHalfBdry = halfBdry2
      }
    })

    if (halfBdry1.size < 3 || halfBdry2.size < 3) {
      println("Empty polygons.  Resetting weights...")
      return None
    }

    val subPoly1 = halfBdry1.toArray
    val subPoly2 = halfBdry2.toArray

    return Some(subPoly1, subPoly2)
  }

  def splitPolygonAtLine(q: Point2D, v: Array[Double], bdry: Array[Point2D]): Option[(Array[Point2D], Array[Point2D])] = {
    val lineEnd1 = new Point2D.Double(q.getX + v(0) * 9999, q.getY + v(1) * 9999)
    val lineEnd2 = new Point2D.Double(q.getX + v(0) * -9999, q.getY + v(1) * -9999)

    var isect1: Point2D = null
    var idxIsect1: Int = -1
    var isect2: Point2D = null
    var idxIsect2: Int = -1
    val uOut = new Array[Double](2)
    (0 until bdry.length).foreach(i => {
      val j = (i + 1) % bdry.length
      getIntersectU(q, lineEnd1, bdry(i), bdry(j), uOut) match {
        case Some(isect) => {
          isect1 = isect
          idxIsect1 = i
        }
        case None => { /*Do nothing*/ }
      }

      getIntersectU(q, lineEnd2, bdry(i), bdry(j), uOut) match {
        case Some(isect) => {
          isect2 = isect
          idxIsect2 = i
        }
        case None => { /*Do nothing*/ }
      }
    })

    if (isect1 == null || isect2 == null)
      return None

    val halfBdry1 = new ArrayBuffer[Point2D]
    val halfBdry2 = new ArrayBuffer[Point2D]
    var activeHalfBdry = halfBdry1

    (0 until bdry.size).foreach(i => {
      activeHalfBdry += bdry(i).clone.asInstanceOf[Point2D]
      if (i == idxIsect1 && activeHalfBdry == halfBdry1) {
        halfBdry1 += isect1
        halfBdry1 += isect2
        activeHalfBdry = halfBdry2
      } else if (i == idxIsect2 && activeHalfBdry == halfBdry2) {
        halfBdry2 += isect2
        halfBdry2 += isect1
        activeHalfBdry = halfBdry1
      } else if (i == idxIsect1 && activeHalfBdry == halfBdry2) {
        halfBdry2 += isect1
        halfBdry2 += isect2
        activeHalfBdry = halfBdry1
      } else if (i == idxIsect2 && activeHalfBdry == halfBdry1) {
        halfBdry1 += isect2
        halfBdry1 += isect1
        activeHalfBdry = halfBdry2
      }
    })

    if (halfBdry1.size < 3 || halfBdry2.size < 3) {
      println("Empty polygons.  Resetting weights...")
      return None
    }

    val subPoly1 = halfBdry1.toArray
    val subPoly2 = halfBdry2.toArray

    return Some(subPoly1, subPoly2)
    //		if (gp1.contains(p1))
    //			return Some(subPoly1, subPoly2)
    //		else
    //			return Some(subPoly2, subPoly1)
  }

  def getVoronoiPolygons(bdry: Array[Point2D], p1: Point2D, p2: Point2D, w1: Double, w2: Double): Option[(Array[Point2D], Array[Point2D])] = {
    // Compute bisector
    val d = p1.distance(p2)
    val t = (d + w1 - w2) / 2.0

    val v = Array(p2.getX - p1.getX, p2.getY - p1.getY)
    val lengthV = math.sqrt(v(0) * v(0) + v(1) * v(1))
    v(0) /= lengthV
    v(1) /= lengthV

    val q = new Point2D.Double(p1.getX + v(0) * t, p1.getY + v(1) * t)
    val vPerp = Array(-v(1), v(0))

    splitPolygonAtLine(q, vPerp, bdry) match {
      case Some((c1, c2)) => {
        val gp1 = getGeneralPath(c1)
        if (gp1.contains(p1))
          return Some(c1, c2)
        else
          return Some(c2, c1)
      }
      case None => return None
    }

    //		val lineEnd1 = new Point2D.Double(q.getX + vPerp(0)*9999, q.getY + vPerp(1)*9999)
    //		val lineEnd2 = new Point2D.Double(q.getX + vPerp(0) * -9999, q.getY + vPerp(1) * -9999)
    //		
    //		var isect1: Point2D = null
    //		var idxIsect1: Int = -1
    //		var isect2: Point2D = null
    //		var idxIsect2: Int = -1
    //		val uOut = new Array[Double](2)
    //		(0 until bdry.length).foreach(i => {
    //			val j = (i + 1) % bdry.length
    //			getIntersectU(q, lineEnd1, bdry(i), bdry(j), uOut) match {
    //				case Some(isect) => {
    //					isect1 = isect
    //					idxIsect1 = i
    //				}
    //				case None => {/*Do nothing*/}
    //			}
    //			
    //			getIntersectU(q, lineEnd2, bdry(i), bdry(j), uOut) match {
    //				case Some(isect) => {
    //					isect2 = isect
    //					idxIsect2 = i
    //				}
    //				case None => {/*Do nothing*/}
    //			}
    //		})
    //		
    //		if (isect1 == null || isect2 == null)
    //			return None
    //		
    //		val halfBdry1 = new ArrayBuffer[Point2D]
    //		val halfBdry2 = new ArrayBuffer[Point2D]
    //		var activeHalfBdry = halfBdry1
    //		
    //		(0 until bdry.size).foreach(i => {
    //			activeHalfBdry += bdry(i).clone.asInstanceOf[Point2D]
    //			if (i == idxIsect1 && activeHalfBdry == halfBdry1) {
    //				halfBdry1 += isect1
    //				halfBdry1 += isect2
    //				activeHalfBdry = halfBdry2
    //			} else if (i == idxIsect2 && activeHalfBdry == halfBdry2) {
    //				halfBdry2 += isect2
    //				halfBdry2 += isect1
    //				activeHalfBdry = halfBdry1
    //			} else if (i == idxIsect1 && activeHalfBdry == halfBdry2) {
    //				halfBdry2 += isect1
    //				halfBdry2 += isect2
    //				activeHalfBdry = halfBdry1
    //			} else if (i == idxIsect2 && activeHalfBdry == halfBdry1) {
    //				halfBdry1 += isect2
    //				halfBdry1 += isect1
    //				activeHalfBdry = halfBdry2
    //			}
    //		})
    //		
    //		if (halfBdry1.size < 3 || halfBdry2.size < 3) {
    //			println("Empty polygons.  Resetting weights...")
    //			return None
    //		}
    //		
    //		val subPoly1 = halfBdry1.toArray
    //		val subPoly2 = halfBdry2.toArray
    //		
    //		val gp1 = getGeneralPath(subPoly1)
    //		if (gp1.contains(p1))
    //			return Some(subPoly1, subPoly2)
    //		else
    //			return Some(subPoly2, subPoly1)
  }

  def getIntersectU(p1: Point2D, p2: Point2D, p3: Point2D, p4: Point2D, uOut: Array[Double]): Option[Point2D] = {
    val denom = (p4.getY - p3.getY) * (p2.getX - p1.getX) - (p4.getX - p3.getX) * (p2.getY - p1.getY)

    if (denom == 0) {
      uOut(0) = 0
      uOut(1) = 0
      return None
    }

    val numeratorA = (p4.getX - p3.getX) * (p1.getY - p3.getY) - (p4.getY - p3.getY) * (p1.getX - p3.getX)
    val numeratorB = (p2.getX - p1.getX) * (p1.getY - p3.getY) - (p2.getY - p1.getY) * (p1.getX - p3.getX)

    val uA = numeratorA / denom
    val uB = numeratorB / denom

    uOut(0) = uA
    uOut(1) = uB
    if (uA < 0 || uA > 1 || uB < 0 || uB > 1)
      return None

    val x = p3.getX + uB * (p4.getX - p3.getX)
    val y = p3.getY + uB * (p4.getY - p3.getY)

    return Some(new Point2D.Double(x, y))
  }

  def getGeneralPath(poly: Array[Point2D]): GeneralPath = {
    val gp = new GeneralPath
    gp.moveTo(poly(0).getX, poly(0).getY)
    (1 until poly.size).foreach(i => {
      gp.lineTo(poly(i).getX, poly(i).getY)
    })
    gp.closePath
    return gp
  }

  def getRandomPointInPolygon(gp: GeneralPath, brect: Rectangle2D): Point2D = {
    val p = new Point2D.Double
    Stream.from(0).foreach(_ => {
      p.x = brect.getX + math.random * brect.getWidth
      p.y = brect.getY + math.random * brect.getHeight
      if (gp.contains(p))
        return p
    })
    null
  }

}
