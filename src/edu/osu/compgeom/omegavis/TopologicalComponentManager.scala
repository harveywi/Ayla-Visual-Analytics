package edu.osu.compgeom.omegavis

import javax.media.j3d.TransformGroup
import javax.media.j3d.BranchGroup
import javax.media.j3d.TriangleStripArray
import edu.osu.compgeom.topology.ScalarFunction
import java.text.DecimalFormat
import java.awt.BasicStroke
import java.awt.AlphaComposite
import java.awt.RenderingHints
import javax.media.j3d.Canvas3D
import org.artemis.progx.graphics.GraphicsUtilities
import java.awt.Color
import javax.media.j3d.LineArray
import javax.media.j3d.TransparencyAttributes
import javax.vecmath.Color4f
import javax.vecmath.Point3f
import javax.media.j3d.{ IndexedTriangleArray, PointArray }
import com.sun.j3d.utils.geometry.Sphere
import javax.media.j3d.Shape3D
import edu.osu.compgeom.ct._
import scala.swing._
import java.awt.geom._
import edu.osu.compgeom.omegavis.event._
import org.jgrapht.graph._
import edu.osu.compgeom.landscapes.floorplan.VoronoiFloorplan
import scala.collection.mutable.ArrayBuffer
import javax.vecmath.Point2d
import javax.vecmath.Point3d
import javax.vecmath.Vector3f
import javax.media.j3d.Transform3D
import edu.osu.compgeom.omegavis.desktop._
import org.jgrapht.alg.DijkstraShortestPath
import scala.collection.JavaConversions._
import javax.media.j3d.GeometryArray
import edu.osu.compgeom.util.HashUnionFind
import edu.osu.compgeom.omegavis
import edu.osu.compgeom.ayla.collab.ConformationAnnotation
import edu.osu.compgeom.ayla.event._

object TopologicalComponentManager {
  val selectionGroupColors = Map(
    1 -> new Color4f(Color.yellow),
    2 -> new Color4f(Color.magenta),
    3 -> new Color4f(Color.green))

  val selectionGroupColorsAWT = Map(1 -> Color.yellow, 2 -> Color.magenta, 3 -> Color.green)
}

/**
 * Serves as the glue between the topological/geometric objects in the model (i.e. point cloud data, contour tree edges/nodes)
 * and their corresponding displayed components in the 3D views.  Also manages selection of different components.
 * @author harveywi
 *
 */
class TopologicalComponentManager(ctPanel: ContourTreePanel, pointCloudView: MutatorPointCloudView, terrainPanel: TerrainPanel) extends Reactor with Publisher {
  var ct: ContourTree = null
  var colormap: GammaColormap = null
  val componentMap = new scala.collection.mutable.HashMap[ContourTreeEdge, TopoComponentEntry]
  var pointArray: PointArray = null
  var terrainSphereBG: BranchGroup = null
  var floorplan: VoronoiFloorplan = null

  val windowLocations = new scala.collection.mutable.HashMap[JmolPalette, Point]

  val hudColorbar = GraphicsUtilities.createTranslucentCompatibleImage(150, 320)
  val selectionInfoImage = GraphicsUtilities.createTranslucentCompatibleImage(512, 200)
  //val selectionGroupColors = Map(1 -> new Color4f(new Color(241, 163, 64)), 2 -> new Color4f(new Color(247, 247, 247)), 3 -> new Color4f(new Color(153, 142, 195)))//Array(new Color(241, 163, 64), new Color(247, 247, 247), new Color(153, 142, 195)).map(new Color4f(_))

  def renderHud(canvas3D: Canvas3D): Unit = {
    val g2d = canvas3D.getGraphics2D()
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    //		g2d.drawAndFlushImage(hudColorbar, 4, canvas3D.getHeight() - 4 - hudColorbar.getHeight(), canvas3D)

    val posColorbar = new Point(10, canvas3D.getHeight() - 10 - hudColorbar.getHeight())
    val rectColorbar = new Rectangle(posColorbar.x, posColorbar.y, hudColorbar.getWidth(), hudColorbar.getHeight())

    val posSelectionInfo = new Point(rectColorbar.x + rectColorbar.width + 4, rectColorbar.y + rectColorbar.height - selectionInfoImage.getHeight())
    val rectSelectionInfo = new Rectangle(posSelectionInfo.x, posSelectionInfo.y, selectionInfoImage.getWidth(), selectionInfoImage.getHeight())
    val dirtyRegion = new Rectangle()
    dirtyRegion.add(rectColorbar)
    dirtyRegion.add(rectSelectionInfo)
    dirtyRegion.x -= 2
    dirtyRegion.y -= 2
    dirtyRegion.width += 4
    dirtyRegion.height += 4

    // Work-around:  Draw an invisible rectangle of empty size which spans the region of all the goodies that will
    // be drawn using 2D graphics.  This will mark the dirty area correctly.
    g2d.setStroke(new BasicStroke(0f))
    g2d.drawRect(dirtyRegion.x, dirtyRegion.y, -1, -1)
    g2d.drawRect(dirtyRegion.x + dirtyRegion.width, dirtyRegion.y + dirtyRegion.height, -1, -1)
    //	  g2d.draw(new Rectangle2D.Float(0, canvas3D.getHeight() - 406, 0, 0))
    //	  g2d.draw(new Rectangle2D.Float(310, canvas3D.getHeight(), 0, 0))
    g2d.drawImage(hudColorbar, posColorbar.x, posColorbar.y, null)

    g2d.drawImage(selectionInfoImage, posSelectionInfo.x, posSelectionInfo.y, null)

    def get3dTo2dPoint(point3d: Point3d): Point2d = {
      val temp = new Transform3D();
      canvas3D.getVworldToImagePlate(temp);
      temp.transform(point3d);
      val point2d = new Point2d();
      canvas3D.getPixelLocationFromImagePlate(point3d, point2d);
      return point2d;
    }

    g2d.setColor(Color.black)

    val r = 6

    windowLocations.foreach{case (palette, windowLocation) if palette.annotation.visible => {
      val annotation = palette.annotation
//      val p3dOpt = floorplan.contourTreeCriticalNodeToTerrainVerts.get(annotation.sampledConformationID) match {
//        case Some(terrainVerts) => {
//          //val tv = terrainVerts.head
//          val tv = terrainVerts((math.random * terrainVerts.length).toInt)
//          Some(new Point3f(floorplan.terrainVertices(tv).x, floorplan.terrainVertices(tv).y, floorplan.terrainVertices(tv).z))
//        }
//        case None => {
//          //floorplan.contourTreeNoncriticalNodesToTerrainVerts.get(annotation.sampledConformationID)
//          floorplan.contourTreeNoncriticalNodesToTerrainVerts.get(annotation.sampledConformationID) match {
//            case Some(verts) => {
//              val tv = verts((math.random * verts.length).toInt)
//              Some(tv)
//            }
//            case None => None
//          }
//        }
//      }
      val p3dOpt: Option[Point3f] = Some(palette.getTerrainVert)

      p3dOpt match {
        case Some(p3f) => {
          val p2d = get3dTo2dPoint(new Point3d(p3f.x, p3f.y, p3f.z))
          //    			  val ellipse = new Ellipse2D.Double(p2d.x-r, p2d.y - r, 2*r, 2*r)
          val mouseCoords = new Point2d(terrainPanel.mouseX, terrainPanel.mouseY)
          if (palette.isChangingOvalLocation) {
            g2d.setStroke(new BasicStroke(2))
            g2d.setColor(Color.orange)
            
            var nearestPt: Point2d = null
            var nearestDistSq = Double.MaxValue
            val r2 = r + 2
            palette.terrainVerts.map(vert => get3dTo2dPoint(new Point3d(vert.p))).foreach{p =>
              val distSq = p.distanceSquared(mouseCoords)
              if (distSq < nearestDistSq) {
                nearestDistSq = distSq
                nearestPt = p
              }
              //g2d.setColor(Color.orange)
              g2d.setColor(new Color(60, 60, 60))
              g2d.drawOval(p.x.toInt - r2, p.y.toInt - r2, 2 * r2, 2 * r2)
            }
            g2d.setColor(Color.black)
            g2d.setStroke(new BasicStroke(6))
            g2d.drawOval(nearestPt.x.toInt - r2, nearestPt.y.toInt - r2, 2 * r2, 2 * r2)
//            g2d.setColor(Color.black)
//            g2d.drawRect(nearestPt.x.toInt - r2, nearestPt.y.toInt - r2, 2 * r2, 2 * r2)
            
          } else {
            
          }
          
          g2d.setStroke(new BasicStroke(2))
          
          g2d.setColor(ColorSchemes.scheme.lineColor)
          g2d.drawOval(p2d.x.toInt - r, p2d.y.toInt - r, 2 * r, 2 * r)

          g2d.setColor(ColorSchemes.scheme.lineColor)
          g2d.setStroke(new BasicStroke(2))
          windowLocations.get(palette) match {
            case Some(windowLocation) => {
              //                val dx = p2d.x - windowLocation.x
              //                val dy = p2d.y - windowLocation.y
              //                if (math.abs(dy) < math.abs(dx)) {
              //                  val horiz = math.abs(dy - dx) * math.signum(dx)
              //                  val b = new Point(windowLocation.x.toInt + horiz.toInt, windowLocation.y.toInt)
              //                  g2d.drawLine(windowLocation.x.toInt, windowLocation.y.toInt, b.x, b.y)
              //                  g2d.drawLine(b.x, b.y, p2d.x.toInt, p2d.y.toInt) 
              //                } else {
              //                  
              //                }

              val xa = windowLocation.x - 50
              val xb = windowLocation.x + 50

              val p1 = if (math.abs(xa - p2d.x) < math.abs(xb - p2d.x)) new Point2D.Double(xa, windowLocation.y) else new Point2D.Double(xb, windowLocation.y)

              val dx = p2d.x - p1.x
              val dy = p2d.y - p1.y
              val elbow1 = new Point2D.Double(p1.x + dx / 2, p1.y)
              val elbow2 = new Point2D.Double(p2d.x, p1.y + dy / 2)

              var vx = p2d.x - elbow2.x
              var vy = p2d.y - elbow2.y
              val norm = math.sqrt(vx * vx + vy * vy)
              if (norm != 0) {
                vx /= norm
                vy /= norm
              }
              val rad = math.max(0, norm - r)
              val theta = math.atan2(vy, vx)

              val elbow3 = new Point2D.Double(elbow2.x + rad * math.cos(theta), elbow2.y + rad * math.sin(theta))

              g2d.drawLine(p1.x.toInt, p1.y.toInt, elbow1.x.toInt, elbow1.y.toInt)
              g2d.drawLine(elbow1.x.toInt, elbow1.y.toInt, elbow2.x.toInt, elbow2.y.toInt)
              g2d.drawLine(elbow2.x.toInt, elbow2.y.toInt, elbow3.x.toInt, elbow3.y.toInt)

              g2d.setColor(ColorSchemes.scheme.bgColor)
              val corner = 20
              val border = 10
              val a = new RoundRectangle2D.Double(windowLocation.x - 50 - border, windowLocation.y - 100 - border, 100 + 2 * border, 200 + 2 * border, corner, corner)
              g2d.fill(a)
              g2d.setColor(ColorSchemes.scheme.lineColor)
              g2d.draw(a)
            }
            case None => {}
          }
          
          val oval = new Ellipse2D.Double(p2d.x.toInt - r, p2d.y.toInt - r, 2*r, 2*r)
          if (oval.contains(mouseCoords.x, mouseCoords.y)) {
            g2d.setColor(Color.black)
            g2d.setStroke(new BasicStroke(4))
            g2d.drawRect(p2d.x.toInt - r, p2d.y.toInt - r, 2*r, 2*r)
          } else {
          	  
          }
          
        }
        case None => {

        }
      }

      //      floorplan.contourTreeCriticalNodeToTerrainVerts.getOrElse(c, floorplan.contourTreeNoncriticalNodesToTerrainVerts.get(c)) match {
      //        case Some(terrainVerts) => {
      //          terrainVerts.take(1).foreach(tv => {
      //            val p3d = new Point3d(floorplan.terrainVertices(tv).x, floorplan.terrainVertices(tv).y, floorplan.terrainVertices(tv).z)
      //            
      //
      //          })
      //        }
      //        case None => {
      //          // This might be a noncritical node
      //          
      //        }
      //      }
    }
    case _ => {}
    }

    //		val img = GraphicsUtilities.createCompatibleImage(canvas3D.getWidth(), canvas3D.getHeight())
    //		val g = img.createGraphics
    //		canvas3D.paint(g)
    //		javax.media.jai.JAI.create("filestore", img, "/dev/shm/img.png", "PNG")
    //		System.gc()
  }

  class TopoComponentEntry(val ctEdge: ContourTreeEdge) {
    private[this] var unsimplifiedCoords: Array[Point3f] = null
    private[this] var unsimplifiedNormals: Array[Vector3f] = null

    private[this] var tc: Shape3D = null
    def terrainComponent_=(newTC: Shape3D) = {
      tc = newTC

      if (terrainComponent == null) {
        unsimplifiedCoords = null
        unsimplifiedNormals = null
      } else {
        val triStrip = terrainComponent.getGeometry().asInstanceOf[TriangleStripArray]
        unsimplifiedCoords = Array.fill(triStrip.getVertexCount())(new Point3f)
        triStrip.getCoordinates(0, unsimplifiedCoords)

        unsimplifiedNormals = Array.fill(triStrip.getVertexCount())(new Vector3f)
        triStrip.getNormals(0, unsimplifiedNormals)
      }

    }
    def terrainComponent = tc

    var ctComponent: Shape3D = null

    def unsimplify(): Unit = {
      if (terrainComponent != null) {
        val triStrip = terrainComponent.getGeometry().asInstanceOf[TriangleStripArray]
        triStrip.setCoordinates(0, unsimplifiedCoords)

        val normals = Array.fill(triStrip.getVertexCount())(new Vector3f)
        triStrip.setNormals(0, unsimplifiedNormals)
      }
    }

    def simplify(newHeight: Float): Unit = {
      val triStrip = terrainComponent.getGeometry().asInstanceOf[TriangleStripArray]
      val coords = Array.fill(triStrip.getVertexCount())(new Point3f)

      triStrip.getCoordinates(0, coords)

      coords.foreach(_.z = newHeight)
      triStrip.setCoordinates(0, coords)

      val normals = Array.fill(triStrip.getVertexCount())(new Vector3f(0, 0, 1))
      triStrip.setNormals(0, normals)
    }

    /**
     * Keeps track of whether or not this component is selected, and if it is, which selection group it belongs to.
     * 0 means the component is not selected; all other integer values indicate that the component is selected.
     */
    var selectionStatus: Int = 0
  }

  def reset(): Unit = {
    componentMap.clear
    colormap = null
    pointArray = null
    terrainSphereBG = null
  }

  reactions += {
    case EdgeSelected(ctEdge, selectionID) => {
      val tcEntry = componentMap(ctEdge)
      if (tcEntry.selectionStatus == 0) {
        tcEntry.selectionStatus = selectionID
        updateSelectionImage()
        if (tcEntry.terrainComponent != null)
          colorTerrainComponent(tcEntry.terrainComponent, tcEntry.selectionStatus)

        if (tcEntry.ctComponent != null)
          colorContourTreeComponent(tcEntry.ctComponent, tcEntry.selectionStatus)

        colorPointCloud()

        publish(new TopologicalComponentManagerSelectionUpdate(this))
      }
    }

    case SelectionCleared() => {
      for ((ctEdge, tcEntry) <- componentMap) {
        if (tcEntry.selectionStatus != 0) {
          tcEntry.selectionStatus = 0
          updateSelectionImage()
          if (tcEntry.terrainComponent != null)
            colorTerrainComponent(tcEntry.terrainComponent, tcEntry.selectionStatus)

          if (tcEntry.ctComponent != null)
            colorContourTreeComponent(tcEntry.ctComponent, tcEntry.selectionStatus)
        }
      }
      colorPointCloud()
      publish(new TopologicalComponentManagerSelectionUpdate(this))
    }

    case TerrainUpdatedEvent(floorplan, ct) => {
      this.floorplan = floorplan
      if (this.ct != ct)
        reset()

      this.ct = ct
      this.terrainSphereBG = floorplan.sphereBranchGroup

      if (this.colormap == null) {
        this.colormap = new JetGammaColormap(ct.scalarFunction)
        updateHudColorbar(ct.scalarFunction)
      }

      floorplan.topoComponents.foreach(s3d => {
        val edge = s3d.getUserData.asInstanceOf[ContourTreeEdge]
        val tcEntry = componentMap.getOrElseUpdate(edge, new TopoComponentEntry(edge))
        tcEntry.terrainComponent = s3d
      })

      for ((ctEdge, tcEntry) <- componentMap) {
        if (tcEntry.terrainComponent != null)
          colorTerrainComponent(tcEntry.terrainComponent, tcEntry.selectionStatus)
      }

      colorTerrainSpheres()

    }

    case ContourTreeViewUpdatedEvent(lines, ct) => {
      if (this.ct != ct)
        reset()

      this.ct = ct

      if (this.colormap == null) {
        this.colormap = new JetGammaColormap(ct.scalarFunction)
        updateHudColorbar(ct.scalarFunction)
      }

      lines.foreach(s3d => {
        val edge = s3d.getUserData.asInstanceOf[ContourTreeEdge]
        val tcEntry = componentMap.getOrElseUpdate(edge, new TopoComponentEntry(edge))
        tcEntry.ctComponent = s3d
      })

      //			ctPanel.detachEdges()
      ctPanel.mutator(() => {
        for ((ctEdge, tcEntry) <- componentMap) {
          if (tcEntry.ctComponent != null) {
            colorContourTreeComponent(tcEntry.ctComponent, tcEntry.selectionStatus)
          }
        }
      })
      //			ctPanel.attachEdges()
    }

//    case e: ConformationPointClicked => {
//      selectedConformations += e.conformationID
//
//      //      if (selectedConformations.size == 2) {
//      //        // Compute path
//      //        val v1 = floorplan.contourTreeCriticalNodeToTerrainVerts(selectedConformations(0)).head
//      //        val v2 = floorplan.contourTreeCriticalNodeToTerrainVerts(selectedConformations(1)).head
//      //        val dsp = new DijkstraShortestPath(floorplan.terrainGraph, v1, v2)
//      //        val pathVerts = dsp.getPathEdgeList.flatMap(e => {
//      //          List(floorplan.terrainVertices(floorplan.terrainGraph.getEdgeSource(e)), floorplan.terrainVertices(floorplan.terrainGraph.getEdgeTarget(e)))
//      //        })
//      //
//      //        val lineArray = new LineArray(pathVerts.size, GeometryArray.COORDINATES)
//      //        lineArray.setCoordinates(0, pathVerts.toArray)
//      //        publish(new PathReady(lineArray))
//      //      }
//    }

    case e: JmolPaletteMoved => {
      windowLocations(e.jmolPalette) = e.newLocation
    }

    case PointCloudViewUpdated(pointArray, ct) => {
      if (this.ct != ct)
        reset()

      this.ct = ct
      this.pointArray = pointArray
      if (this.colormap == null) {
        this.colormap = new JetGammaColormap(ct.scalarFunction)
        updateHudColorbar(ct.scalarFunction)
      }

      ct.criticalNodeToIncidentEdges.values.foreach(edgeList => {
        edgeList.foreach(e => {
          componentMap.getOrElseUpdate(e, new TopoComponentEntry(e))
        })
      })

      colorPointCloud()
    }

    case ColormapUpdate(newColormap, sf) => {
      this.colormap = newColormap

      updateHudColorbar(sf)

      //			ctPanel.detachEdges()
      terrainPanel.mutator(() =>
        {
          for ((ctEdge, tcEntry) <- componentMap) {
            if (tcEntry.terrainComponent != null)
              colorTerrainComponent(tcEntry.terrainComponent, tcEntry.selectionStatus)

//            if (tcEntry.ctComponent != null)
//              colorContourTreeComponent(tcEntry.ctComponent, tcEntry.selectionStatus)
          }
        })
      //			ctPanel.attachEdges()
      colorPointCloud()
    }

    case SimplifyContourTree(tau) => {
      // Find all the persistence pairs that are within the cancellation threshold
      val persPairs = ct.scalarFunction.getStandardPersistencePairs.filter(_.persistence < tau)

      // Build a couple graphs so we can find paths connected each extremum with its paired saddle
      val graph = new SimpleGraph[Int, DefaultEdge](classOf[DefaultEdge])
      ct.nodesContracted.foreach(n => graph.addVertex(n.vertex))
      ct.nodesContracted.foreach(n => n.parents.foreach(p => graph.addEdge(n.vertex, p.vertex)))

      // Find the path connecting each cancelled saddle-extremum pair
      //      val maxPairs = persPairs.filter(pair => ct.nodesAugmented(pair.extremum).isMax)
      //      val minPairs = persPairs.filter(pair => ct.nodesAugmented(pair.extremum).isMin)
      //      assert(maxPairs.size + minPairs.size == persPairs.size)

      val ctEdgeMap = ct.criticalNodeToIncidentEdges.values.flatten.map(e => {
        val vertPair = if (ct.scalarFunction.vc.compare(e.n1.vertex, e.n2.vertex) < 0) (e.n1.vertex, e.n2.vertex) else (e.n2.vertex, e.n1.vertex)
        vertPair -> e
      }).toMap

      val uf = new HashUnionFind[Int]
      val traversedEdges = new scala.collection.mutable.HashSet[ContourTreeEdge]

      //      List((minPairs, upGraph), (maxPairs, downGraph)).foreach {
      //        case (pairs, graph) => {
      persPairs.foreach { pair =>
        val dsp = new DijkstraShortestPath(graph, pair.extremum, pair.saddle)
        val pathVerts = dsp.getPathEdgeList.flatMap(graphEdge => {
          val v1 = graph.getEdgeSource(graphEdge)
          val v2 = graph.getEdgeTarget(graphEdge)
          uf.union(v1, v2)
          val ctEdge = if (ct.scalarFunction.vc.compare(v1, v2) < 0) {
            ctEdgeMap((v1, v2))
          } else {
            ctEdgeMap((v2, v1))
          }
          traversedEdges += ctEdge
        })
      }

      val nontraversedEdges = ct.criticalNodeToIncidentEdges.values.flatten.filterNot(traversedEdges.contains)

      //        }
      //      }

      val stack = new scala.collection.mutable.Stack[Int]
      ct.nodesContracted.filter(n => uf.findOpt(n.vertex).isDefined).foreach(n => if (n.isMax || n.isMin) stack.push(n.vertex))
      while (!stack.isEmpty) {
        val v1 = stack.pop
        assert(uf.findOpt(v1).isDefined)
        val setID = uf.find(v1)
        val e = graph.edgesOf(v1).iterator().next
        val v2 = if (v1 == graph.getEdgeSource(e)) graph.getEdgeTarget(e) else graph.getEdgeSource(e)
        graph.removeEdge(e)
        graph.removeVertex(v1)
        if (graph.edgesOf(v2).size == 1)
          stack.push(v2)
      }

      val clingMap = graph.vertexSet().flatMap { v =>
        uf.findOpt(v) match {
          case Some(setID) => Some(setID -> v)
          case None => None
        }
      }.toMap

      //      //      var a = 0
      //      //      var b = 0
      //      //      var c = 0
      //      //      var d = 0
      //      //      var f = 0
      //      //      ct.criticalNodeToIncidentEdges.values.flatten.foreach{e =>
      //      //        (uf.findOpt(e.n1.vertex), uf.findOpt(e.n2.vertex)) match {
      //      //          case (Some(p), Some(q)) => {a += 1; if (p == q) f += 1}
      //      //          case (Some(_), None) => {b += 1}
      //      //          case (None, Some(_)) => {c += 1}
      //      //          case (None, None) => {d += 1}
      //      //        }
      //      //      }
      //      //      
      //      //      println(List(a,b,c,d,f).mkString("", ",", "\n"))
      //
      //      // Now each connected component of the subset of the contour tree that will be canceled
      //      // will be clinging to the rest of the tree at exactly one node.  We will find that node here.
      //      val clingNodeMap = new scala.collection.mutable.HashMap[Int, Int]
      //      val edgesGrouped = traversedEdges.groupBy { e =>
      //        Iterator(e.n1, e.n2).filter(n => uf.findOpt(n.vertex).isDefined).foreach { n =>
      //          val setID = uf.find(n.vertex)
      //          (n.parents.iterator ++ n.children.iterator).find(neighbor => !uf.findOpt(neighbor.vertex).isDefined) match {
      //            case Some(unmarkedNeighbor) => {
      //              if (clingNodeMap.isDefinedAt(setID)) {
      //                println("Whacking " + clingNodeMap(setID) + " and replacing it with " + n.vertex)
      //              }
      //              clingNodeMap(setID) = n.vertex
      //            }
      //            case None => {
      //
      //            }
      //          }
      //        }
      //      }

      terrainPanel.mutator(() => {

        nontraversedEdges.foreach { e =>
          componentMap.get(e) match {
            case Some(tc) if tc.terrainComponent != null => {
              // Reset z coordinate and normals to defaults
              tc.unsimplify()
            }
            case _ => { /* Do nothing */ }
          }
        }

        traversedEdges.foreach { e =>
          var height = Float.MaxValue

          Iterator(e.n1.vertex, e.n2.vertex).foreach { v =>
            if (uf.findOpt(v).isDefined) {
              val setID = uf.find(v)
              val clingID = clingMap(setID)
              height = floorplan.terrainVertices(floorplan.contourTreeCriticalNodeToTerrainVerts(clingID)(0)).z
            }
          }

          assert(height != Float.MaxValue)

          componentMap.get(e) match {
            case Some(tc) if tc.terrainComponent != null => {
              tc.simplify(height)
            }
            case _ => { /* Do nothing */ }
          }
        }
      })

      //      potentialClearNodes.foreach{clearNode =>
      //        val x = floorplan.contourTreeCriticalNodeToTerrainVerts(clearNode)
      ////        componentMap
      //      }

      //      val ctNodesToClear = new scala.collection.mutable.HashSet[Int]
      //      val x = floorplan.contourTreeCriticalNodeToTerrainVerts
      // Now 
    }
  }

  /*
	def getNodeColor(f: Float, tcEntry: TopoComponentEntry): Color4f = {
		val sf = ct.scalarFunction
		val yellow = new Color4f(Color.yellow)
		if (tcEntry.selected)
			return yellow
		val ctEdge = tcEntry.ctEdge
		val h1 = sf.getFuncVal(ctEdge.n1.vertex)
		val h2 = sf.getFuncVal(ctEdge.n2.vertex)
		val hMin = math.min(h1, h2)
		val hMax = math.max(h1, h2)
		val alpha = (f - h2) / (h1 - h2)
		colormap.getColor(ctEdge, alpha)
	}
	*/

  def colorPointCloud(): Unit = {
    val pointToTopoComponent = new Array[TopoComponentEntry](ct.scalarFunction.vertices.size)
    for ((ctEdge, tcEntry) <- componentMap) {
      pointToTopoComponent(ctEdge.n1.vertex) = tcEntry
      pointToTopoComponent(ctEdge.n2.vertex) = tcEntry
      ctEdge.noncriticalNodes.foreach(n => { pointToTopoComponent(n.vertex) = tcEntry })
    }
    //		ct.criticalNodeToIncidentEdges.values.flatten.foreach(ctEdge => {
    //			pointToContourTreeEdge(ctEdge.n1.vertex) = ctEdge
    //			pointToContourTreeEdge(ctEdge.n2.vertex) = ctEdge
    //			ctEdge.noncriticalNodes.foreach(n => {pointToContourTreeEdge(n.vertex) = ctEdge})
    //		})

    def getNodeColor(f: Float, tcEntry: TopoComponentEntry): Color4f = {
      val sf = ct.scalarFunction
      if (tcEntry.selectionStatus != 0)
        return TopologicalComponentManager.selectionGroupColors(tcEntry.selectionStatus)
      val ctEdge = tcEntry.ctEdge
      val h1 = sf.getFuncVal(ctEdge.n1.vertex)
      val h2 = sf.getFuncVal(ctEdge.n2.vertex)
      val hMin = math.min(h1, h2)
      val hMax = math.max(h1, h2)
      val alpha = (f - h2) / (h1 - h2)
      colormap.getColor(ctEdge, alpha)
    }

    val colors = ct.scalarFunction.vertices.indices.map(i => {
//      if (pointToTopoComponent(i) != null) {
        getNodeColor(ct.scalarFunction.getFuncVal(i), pointToTopoComponent(i))
//      } else {
//        new Color4f
//      }
    }).toArray

    pointCloudView.mutator(() => { pointArray.setColors(0, colors) })
  }

  def colorTerrainSpheres(): Unit = {
    val pointToTopoComponent = new Array[TopoComponentEntry](ct.scalarFunction.vertices.size)
    for ((ctEdge, tcEntry) <- componentMap) {
      pointToTopoComponent(ctEdge.n1.vertex) = tcEntry
      pointToTopoComponent(ctEdge.n2.vertex) = tcEntry
      ctEdge.noncriticalNodes.foreach(n => { pointToTopoComponent(n.vertex) = tcEntry })
    }
    //		ct.criticalNodeToIncidentEdges.values.flatten.foreach(ctEdge => {
    //			pointToContourTreeEdge(ctEdge.n1.vertex) = ctEdge
    //			pointToContourTreeEdge(ctEdge.n2.vertex) = ctEdge
    //			ctEdge.noncriticalNodes.foreach(n => {pointToContourTreeEdge(n.vertex) = ctEdge})
    //		})

    val sf = ct.scalarFunction
    def getNodeColor(f: Float, tcEntry: TopoComponentEntry): Color4f = {
      if (tcEntry.selectionStatus != 0)
        return TopologicalComponentManager.selectionGroupColors(tcEntry.selectionStatus)
      val ctEdge = tcEntry.ctEdge
      val h1 = sf.getFuncVal(ctEdge.n1.vertex)
      val h2 = sf.getFuncVal(ctEdge.n2.vertex)
      val hMin = math.min(h1, h2)
      val hMax = math.max(h1, h2)
      val alpha = (f - h2) / (h1 - h2)
      //			colormap.getColor(f)
      colormap.getColor(ctEdge, alpha)
    }

    val terrainSphereBGChildren = terrainSphereBG.getAllChildren()
    while (terrainSphereBGChildren.hasMoreElements()) {
      val sphere = terrainSphereBGChildren.nextElement().asInstanceOf[TransformGroup].getChild(0).asInstanceOf[Sphere]
      val shape = sphere.getShape()
      val n = shape.getUserData().asInstanceOf[ContourTreeNode]
      val color = getNodeColor(sf.getFuncVal(n.vertex), pointToTopoComponent(n.vertex))

      val material = shape.getAppearance().getMaterial()
      material.setDiffuseColor(color.x, color.y, color.z)
      shape.getAppearance().setMaterial(material)

      sphere.getAppearance().setMaterial(material)

      //			val appearance = sphere.getShape().getAppearance()
      //			val material = appearance.getMaterial()
      //			val n = sphere.getShape().getUserData().asInstanceOf[ContourTreeNode]
      //			val color = getNodeColor(sf.getFuncVal(n.vertex), pointToTopoComponent(n.vertex))
      //			material.setDiffuseColor(color.x, color.y, color.z)
      //			appearance.setMaterial(material)
      //			sphere.setAppearance(appearance)
      //			sphere.getShape().setAppearance(appearance)
      //			sphere.getShape().getAppearance().setMaterial(material)
      val z = 3
    }
    //		terrainSphereBG.getAllChildren().map(_.asInstanceOf[TransformGroup])

    //		val colors = ct.scalarFunction.vertices.indices.map(i => {
    //			getNodeColor(sf.getFuncVal(i), pointToTopoComponent(i))
    //		}).toArray
    //		println(colors.size)
    //		
    //		pointArray.setColors(0, colors)		
  }

  def colorContourTreeComponent(component: Shape3D, selectionID: Int): Unit = {
    val line = component.getGeometry.asInstanceOf[LineArray]
    val e = component.getUserData.asInstanceOf[ContourTreeEdge]

    val coords = Array.fill[Point3f](2)(new Point3f)
    line.getCoordinates(0, coords)

    val y1 = coords(0).y
    val y2 = coords(1).y

    val (alpha1, alpha2) = if (y1 < y2) (1d, 0d) else (0d, 1d)
    val (c1, c2) = if (selectionID == 0)
      (colormap.getColor(e, alpha1), colormap.getColor(e, alpha2))
    else
      (TopologicalComponentManager.selectionGroupColors(selectionID), TopologicalComponentManager.selectionGroupColors(selectionID))
    //		val c1 = colormap(e, alpha1)
    //		val c2 = colormap(e, alpha2)
    line.setColors(0, Array(c1, c2))
  }

  def colorTerrainComponent(component: Shape3D, selectionID: Int): Unit = {
    val geomArray = component.getGeometry.asInstanceOf[TriangleStripArray]
    val terrainCoords = Array.fill[Point3f](geomArray.getVertexCount)(new Point3f)
    geomArray.getCoordinates(0, terrainCoords)
    val ctEdge = component.getUserData.asInstanceOf[ContourTreeEdge]

    var zMin = Float.MaxValue
    var zMax = Float.MinValue
    var foundTransparent = false
    terrainCoords.foreach { tc =>
      zMin = math.min(zMin, tc.z)
      zMax = math.max(zMax, tc.z)
    }

    val colors = if (selectionID == 0) terrainCoords.map(v => {
      val alpha = (v.z - zMax) / (zMin - zMax)
      val color = colormap.getColor(ctEdge, alpha)
      if (color.w < 1)
        foundTransparent = true
      color
    }).toArray
    else Array.fill(terrainCoords.size)(TopologicalComponentManager.selectionGroupColors(selectionID))
    geomArray.setColors(0, colors)

    val transAtts = component.getAppearance().getTransparencyAttributes()
    if (foundTransparent) {
      transAtts.setTransparencyMode(TransparencyAttributes.NICEST)
      transAtts.setTransparency(0)
    } else {
      transAtts.setTransparencyMode(TransparencyAttributes.NONE)
      transAtts.setTransparency(0)
    }
  }

  /*
  def colorTerrainComponent(component: Shape3D, selectionID: Int): Unit = {
    val geomArray = component.getGeometry.asInstanceOf[TriangleStripArray]
    val terrainCoords = Array.fill[Point3f](geomArray.getVertexCount)(new Point3f)
    geomArray.getCoordinates(0, terrainCoords)
    val zVals = terrainCoords.map(_.z)
    val zMin = zVals.min
    val zMax = zVals.max
    val ctEdge = component.getUserData.asInstanceOf[ContourTreeEdge]

    val colors = if (selectionID == 0) terrainCoords.map(v => {
      val alpha = (v.z - zMax) / (zMin - zMax)
      colormap.getColor(ctEdge, alpha)
    }).toArray
    else Array.fill(terrainCoords.size)(TopologicalComponentManager.selectionGroupColors(selectionID))
    geomArray.setColors(0, colors)
    //		geomArray.setColorIndices(0, colors.indices.toArray)

    // Decide whether or not to enable transparency here
    val transAtts = component.getAppearance().getTransparencyAttributes()
    colors.find(_.w < 1) match {
      case Some(color) => {
        transAtts.setTransparencyMode(TransparencyAttributes.NICEST)
        transAtts.setTransparency(0)
      }
      case None => {
        transAtts.setTransparencyMode(TransparencyAttributes.NONE)
        transAtts.setTransparency(0)
      }
    }
  }
  */

  def getFuncValHistogram(nBins: Int, sf: ScalarFunction): Array[Float] = {
    val binWidth = sf.rangeFuncVal / nBins.toInt
    val hist = new Array[Float](nBins)
    sf.vertices.indices.foreach(i => {
      val f = sf.getFuncVal(i)
      val binNum = math.min(nBins * (f - sf.minFuncVal) / sf.rangeFuncVal, nBins - 1).toInt
      hist(binNum) += 1f
    })

    // Normalize the histogram
    hist.indices.foreach(i => {
      hist(i) /= sf.vertices.size.toFloat
    })

    hist
  }

  def updateSelectionImage(): Unit = {
    val g2d = selectionInfoImage.createGraphics()
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    // Clear the old image
    val oldComposite = g2d.getComposite
    g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.CLEAR, 0.0f))
    g2d.fillRect(0, 0, selectionInfoImage.getWidth(), selectionInfoImage.getHeight())
    g2d.setComposite(oldComposite)

    g2d.setColor(Color.white)
    //		g2d.drawRect(0, 0, selectionInfoImage.getWidth()-2, selectionInfoImage.getHeight()-2)
  }

  def updateHudColorbar(sf: ScalarFunction): Unit = {
    val vertInset = 1
    val nBars = 16
    val barRegionHeight = hudColorbar.getHeight() / nBars
    val minBarWidth = 4
    val maxBarWidth = 70

    // Compute a histogram of the data
    val hist = getFuncValHistogram(nBars, sf).reverse
    val histMin = hist.min
    val histMax = hist.max

    val g2d = hudColorbar.createGraphics()
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    // Clear the old image
    val oldComposite = g2d.getComposite
    g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.CLEAR, 0.0f))
    g2d.fillRect(0, 0, hudColorbar.getWidth(), hudColorbar.getHeight())
    g2d.setComposite(oldComposite)

    /*
		g2d.setColor(Color.white)
		g2d.drawRect(0, 0, hudColorbar.getWidth()-1, hudColorbar.getHeight()-2)
		*/

    // Draw the bars
    val decFormat = new DecimalFormat("00.00E0")
    val fRange = colormap.rangeMax - colormap.rangeMin
    for (i <- 0 until nBars) {
      // Figure out what the function values are that bound the bar
      val barTop = nBars - i
      val barBottom = nBars - (i + 1)
      val f = colormap.rangeMin + fRange * ((barTop + barBottom) / 2f / nBars.toFloat)
      val c4f = colormap.getColor(f)

      // Figure out the width of the bar based on the histogram
      val barWidth = minBarWidth + ((hist(i) - histMin) / (histMax - histMin) * (maxBarWidth - minBarWidth)).toInt

      val rect = new RoundRectangle2D.Double(0, i * barRegionHeight + vertInset, barWidth, barRegionHeight - 2 * vertInset, 3, 3)
      val cInner = new Color(c4f.x, c4f.y, c4f.z, .6f)
      g2d.setColor(cInner)
      g2d.fill(rect)

      val cOuter = new Color(c4f.x, c4f.y, c4f.z, c4f.w)
      g2d.setColor(cOuter)
      g2d.draw(rect)

      g2d.setColor(cOuter)
      val s = decFormat.format(f)
      g2d.drawString(s, (rect.x + rect.width + 6).toInt, (rect.y + rect.height - 4).toInt)
    }

    //Rectangle2D.Double rect = 
    //  new Rectangle2D.Double(0,0,width,height); 
    //g2D.fill(rect);

    //		g2d.clearRect(0, 0, hudColorbar.getWidth(), hudColorbar.getHeight())
    g2d.setColor(Color.white)

    //		g2d.setColor(Color.white)
    //		g2d.drawRect(0, 0, hudColorbar.getWidth(), hudColorbar.getHeight())
    //		g2d.drawString("Hello", 10, 10)
  }

}