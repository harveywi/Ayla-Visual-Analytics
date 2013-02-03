/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.collab

import scala.swing._
import scala.swing.event._
import javax.swing.ImageIcon
import java.awt.geom._
import java.awt.RenderingHints
import org.artemis.progx.graphics.GraphicsUtilities
import java.awt.RenderingHints
import StoryboardPanel._
import javax.swing.BorderFactory
import javax.swing.border.TitledBorder
import javax.swing.UIManager
import java.awt.{ Color, BasicStroke }
import javax.swing.TransferHandler
import java.awt.datatransfer._
import javax.swing.JComponent
import javax.swing.Box
import ayla.client.AylaClient
import ayla.client.ui.event._
import ayla.landscape._
import javax.vecmath._
import org.jgrapht.alg.DijkstraShortestPath
import scala.collection.JavaConverters._
import javax.media.j3d.LineArray
import javax.media.j3d.GeometryArray
import ayla.client.ui.desktop.DesktopPane
import org.jgrapht.graph._
import javax.swing.JPopupMenu
import javax.swing.JMenuItem
import java.awt.event.ActionListener
import ayla.client.ui.JmolPanel
import javax.media.j3d._
import com.sun.j3d.utils.geometry.Sphere
import ayla.client.ui.ColorSchemes.scheme

class StoryboardPanel(client: AylaClient, collabFrame: AylaCollaborationFrame) extends BorderPanel {
  var floorplan: VoronoiFloorplan = null
  var desktopPane: DesktopPane = null

  reactions += {
    case TerrainUpdatedEvent(floorplan, ct) => {
      StoryboardPanel.this.floorplan = floorplan
    }
  }

  val savedStoryboardsListView = new ListView[Storyboard] {
    listenTo(mouse.clicks)
    background = scheme.bgColor
    foreground = scheme.btnForeground

    reactions += {
      case e: MouseClicked => {
        val i = peer.locationToIndex(e.point)
        if (i != -1) {
          val listItem = listData(i)
          storyboardEditor.contents.remove(1, storyboardEditor.contents.size - 1)
          listItem.annotations.foreach { annotation =>
            storyboardEditor.contents += new AnnotationArea(annotation)
            storyboardEditor.contents += new DropArea
          }
          storyboardEditor.revalidate()
          storyboardEditor.repaint()
        }
      }
    }
  }

  val controlPanel = new BorderPanel {
    var movieIdx = -1
    var activeAnnotation: ConformationAnnotation = null
    background = scheme.bgColor
    foreground = scheme.btnForeground
    add(new FlowPanel {
      background = scheme.bgColor
      foreground = scheme.btnForeground

      // back
      contents += new Button {
        background = scheme.btnBackground
        foreground = scheme.btnForeground
        icon = backIcon
        this.borderPainted = false
        reactions += {
          case e: ButtonClicked => {
            val annotations = storyboardEditor.contents.flatMap {
              _ match {
                case area: AnnotationArea => Some(area.annotation)
                case _ => None
              }
            }.toArray
            if (annotations.isEmpty) {
              movieIdx = -1
              activeAnnotation = null
            } else {
              movieIdx = math.max(movieIdx, 0)
              movieIdx -= 1
              if (movieIdx < 0) {
                movieIdx += annotations.size
              }
              annotations.foreach(_.visible = false)
              annotations(movieIdx).visible = true
              activeAnnotation = annotations(movieIdx)
              collabFrame.publish(RefreshAnnotationVisibilities)
            }
            collabFrame.annotationListView.listData.foreach { item =>
              item.selected = item.annotation.visible
            }
            collabFrame.annotationListView.repaint()
            storyboardEditor.repaint()
          }
        }
      }

      // stop
      contents += new Button {
        background = scheme.btnBackground
        foreground = scheme.btnForeground
        icon = stopIcon
        this.borderPainted = false
        reactions += {
          case e: ButtonClicked => {
            val annotations = storyboardEditor.contents.flatMap {
              _ match {
                case area: AnnotationArea => Some(area.annotation)
                case _ => None
              }
            }.toArray
            if (!annotations.isEmpty) {
              annotations.foreach(_.visible = false)
              //              annotations.head.visible = true
              //              activeAnnotation = annotations.head
              collabFrame.publish(RefreshAnnotationVisibilities)
              //              movieIdx = 0
              movieIdx = -1
              activeAnnotation = null
            } else {
              movieIdx = -1
              activeAnnotation = null
            }
            collabFrame.annotationListView.listData.foreach { item =>
              item.selected = item.annotation.visible
            }
            collabFrame.annotationListView.repaint()
            storyboardEditor.repaint()
          }
        }
      }

      // forward
      contents += new Button {
        background = scheme.btnBackground
        foreground = scheme.btnForeground
        icon = fwdIcon
        this.borderPainted = false
        reactions += {
          case e: ButtonClicked => {
            val annotations = storyboardEditor.contents.flatMap {
              _ match {
                case area: AnnotationArea => Some(area.annotation)
                case _ => None
              }
            }.toArray
            if (annotations.isEmpty) {
              movieIdx = -1
              activeAnnotation = null
            } else {
              movieIdx = (movieIdx + 1) % annotations.size
              annotations.foreach(_.visible = false)
              annotations(movieIdx).visible = true
              activeAnnotation = annotations(movieIdx)
              collabFrame.publish(RefreshAnnotationVisibilities)
            }
            collabFrame.annotationListView.listData.foreach { item =>
              item.selected = item.annotation.visible
            }
            collabFrame.annotationListView.repaint()
            storyboardEditor.repaint()
          }
        }
      }
    }, BorderPanel.Position.Center)

    add(new Button("Upload Storyboard") {
      background = scheme.btnBackground
      foreground = scheme.btnForeground
      reactions += {
        case e: ButtonClicked => {
          println("Committing storyboard.")
          val annotations = storyboardEditor.contents.flatMap {
            _ match {
              case area: AnnotationArea => Some(area.annotation)
              case _ => None
            }
          }.toArray

          if (annotations.isEmpty) {
            Dialog.showMessage(message = "Error:  The storyboard is empty.")
          } else {
            Dialog.showInput(message = "Storyboard Name", initial = "") match {
              case Some(name) => {
                val storyboard = new Storyboard(name, annotations)
                client.postStoryboard(storyboard)
              }
              case None => {}
            }
          }
        }
      }
    }, BorderPanel.Position.East)
  }

  val savedStoryboardsScrollPane = new ScrollPane(savedStoryboardsListView) {
    background = scheme.bgColor
    border = BorderFactory.createTitledBorder(BorderFactory.createLineBorder(scheme.lineColor), "Storyboards",
      TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, UIManager.getFont("TitledBorder.font"), scheme.btnForeground)
    minimumSize = new Dimension(100, 50)
  }
  add(savedStoryboardsScrollPane, BorderPanel.Position.West)

  //  val storyboardEditor = new FlowPanel(FlowPanel.Alignment.Left)() {
  val storyboardEditor: BoxPanel = new BoxPanel(Orientation.Horizontal) {

    listenTo(this)
    background = scheme.bgColor
    preferredSize = new Dimension(50, 50)
    contents += new DropArea
    reactions += {
      case e: ComponentAdded => {
        controlPanel.movieIdx = -1
        controlPanel.activeAnnotation = null
        val w = contents.map(_.preferredSize.width).sum
        val h = contents.map(_.preferredSize.height).max
        preferredSize = new Dimension(w, h)
      }
      case e: ComponentRemoved => {
        controlPanel.movieIdx = -1
        controlPanel.activeAnnotation = null
        val w = contents.map(_.preferredSize.width).sum
        val h = contents.map(_.preferredSize.height).max
        preferredSize = new Dimension(w, h)
      }
    }
  }
  val storyboardEditorScrollPane = new ScrollPane(storyboardEditor) {
    verticalScrollBarPolicy = ScrollPane.BarPolicy.Never
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
    background = scheme.bgColor
    border = BorderFactory.createTitledBorder(BorderFactory.createLineBorder(scheme.lineColor), "Storyboard Editor",
      TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, UIManager.getFont("TitledBorder.font"), scheme.btnForeground)
    minimumSize = new Dimension(100, 50)
  }

  add(storyboardEditorScrollPane, BorderPanel.Position.Center)

  add(controlPanel, BorderPanel.Position.South)

  class DropArea extends Component with ActionListener {
    preferredSize = new Dimension(12, 60)
    minimumSize = new Dimension(12, 60)
    maximumSize = new Dimension(12, 60)

    val popup = new JPopupMenu()
    val menuShowPath = new JMenuItem("Show Path")
    val menuCompareProteins = new JMenuItem("Compare Conformations")
    menuShowPath.addActionListener(this)
    menuCompareProteins.addActionListener(this)
    popup.add(menuShowPath)
    popup.add(menuCompareProteins)

    listenTo(mouse.clicks)

    override def actionPerformed(e: java.awt.event.ActionEvent): Unit = {
      if (e.getSource == menuShowPath) {
        println("Showing pathway")
        val myIndex = storyboardEditor.contents.indexOf(DropArea.this)
        println("My index is:  " + myIndex)
        if (myIndex != 0 && myIndex < storyboardEditor.contents.size - 1) {
          println("I'm in bounds.")
          val left = storyboardEditor.contents(myIndex - 1).asInstanceOf[AnnotationArea].annotation
          val right = storyboardEditor.contents(myIndex + 1).asInstanceOf[AnnotationArea].annotation

          val leftJmolPalette = desktopPane.jmolPalettes.find(_.annotation == left).get
          val rightJmolPalette = desktopPane.jmolPalettes.find(_.annotation == right).get

          val sources = leftJmolPalette.terrainVerts(leftJmolPalette.terrainVertexID) match {
            case pos: NoncriticalPointTerrainPos => {
              Array(pos.terrainIdx1, pos.terrainIdx2)
            }
            case pos: CriticalPointTerrainPos => {
              Array(pos.terrainIdx)
            }
          }

          val sinks = rightJmolPalette.terrainVerts(rightJmolPalette.terrainVertexID) match {
            case pos: NoncriticalPointTerrainPos => {
              Array(pos.terrainIdx1, pos.terrainIdx2)
            }
            case pos: CriticalPointTerrainPos => {
              Array(pos.terrainIdx)
            }
          }

          // Have to try all pairs of sources and sinks
          var bestPathLength = Double.MaxValue
          var bestPath: DijkstraShortestPath[Int, DefaultWeightedEdge] = null
          sources.foreach { source =>
            sinks.foreach { sink =>
              println("Running dijkstra on " + source + " and " + sink)
              val dsp = new DijkstraShortestPath(floorplan.terrainGraph, source, sink)
              val pathLength = dsp.getPathLength()
              if (pathLength < bestPathLength) {
                bestPathLength = pathLength
                bestPath = dsp
              }
            }
          }

          println("sources:  " + sources.mkString(","))
          println("sinks:  " + sinks.mkString(","))
          println("Best path length:  " + bestPathLength)
          println("floorplan:  " + floorplan)
          println("Terrain graph verts:" + floorplan.terrainGraph.vertexSet().size)
          println("Terrain graph edges:" + floorplan.terrainGraph.edgeSet().size)
          println("Best path:  " + bestPath.toString())

          val pathVerts = bestPath.getPathEdgeList.asScala.flatMap(e => {
            List(floorplan.terrainVertices(floorplan.terrainGraph.getEdgeSource(e)), floorplan.terrainVertices(floorplan.terrainGraph.getEdgeTarget(e)))
          })

          if (!pathVerts.isEmpty) {
            if (sources.length > 1) {
              val oldHead = pathVerts.head
              pathVerts.prepend(leftJmolPalette.getTerrainVert, oldHead)
            }

            if (sinks.length > 1) {
              val oldLast = pathVerts.last
              pathVerts.append(oldLast, rightJmolPalette.getTerrainVert)
            }

            val lineArray = new LineArray(pathVerts.size, GeometryArray.COORDINATES)
            lineArray.setCoordinates(0, pathVerts.toArray)
            StoryboardPanel.this.publish(new PathReady(lineArray))

            // TODO compute the true shortest path in high-dimensional space, and show those conformations as dots over the landscape
            // But we only want to keep shortest path conformations which are actually on the terrain path...

            @inline def ptSegDistSq(x_in: Point3f, y_in: Point3f, p_in: Point3f): Double = {
              val x = new Vector3f(x_in)
              val y = new Vector3f(y_in)
              val p = new Vector3f(p_in)
              // Adjust vectors relative to x1,y1
              // x2,y2 becomes relative vector from x1,y1 to end of segment
              // px,py becomes relative vector from x1,y1 to test point           
              y.sub(x)
              p.sub(x)

              var dotprod: Double = y.dot(p) //px * x2 + py * y2;
              var projlenSq = 0d;
              if (dotprod <= 0.0) {
                // px,py is on the side of x1,y1 away from x2,y2
                // distance to segment is length of px,py vector
                // "length of its (clipped) projection" is now 0.0
                projlenSq = 0.0;
              } else {
                // switch to backwards vectors relative to x2,y2
                // x2,y2 are already the negative of x1,y1=>x2,y2
                // to get px,py to be the negative of px,py=>x2,y2
                // the dot product of two negated vectors is the same
                // as the dot product of the two normal vectors
                p.x = y.x - p.x
                p.y = y.y - p.y
                p.z = y.z - p.z
                //	    px = x2 - px;
                //	    py = y2 - py;
                dotprod = p.dot(y)
                if (dotprod <= 0.0) {
                  // px,py is on the side of x2,y2 away from x1,y1
                  // distance to segment is length of (backwards) px,py vector
                  // "length of its (clipped) projection" is now 0.0
                  projlenSq = 0.0;
                } else {
                  // px,py is between x1,y1 and x2,y2
                  // dotprod is the length of the px,py vector
                  // projected on the x2,y2=>x1,y1 vector times the
                  // length of the x2,y2=>x1,y1 vector
                  projlenSq = dotprod * dotprod / y.dot(y) //(x2 * x2 + y2 * y2);
                }
              }
              // Distance to line is now the length of the relative point
              // vector minus the length of its projection onto the line
              // (which is zero if the projection falls outside the range
              //  of the line segment).
              //	var lenSq = px * px + py * py - projlenSq;
              var lenSq = p.dot(p) - projlenSq
              if (lenSq < 0) {
                lenSq = 0;
              }
              return lenSq;
            }
            
//            AylaClient.ct.
            
            val ctShortestPathVerts = {
              val ctGraph = new SimpleGraph[Int, DefaultEdge](classOf[DefaultEdge])
	            val ct = client.ct
	            ct.nodesAugmented.foreach(n => ctGraph.addVertex(n.vertex))
	            ct.nodesAugmented.foreach{n =>
	              n.parents.foreach{parent =>
	                ctGraph.addEdge(n.vertex, parent.vertex)
	              }
	            }
              val dsp = new DijkstraShortestPath(ctGraph, leftJmolPalette.annotation.sampledConformationID, rightJmolPalette.annotation.sampledConformationID)
              val verts = new scala.collection.mutable.ArrayBuffer[Int]
              verts += leftJmolPalette.annotation.sampledConformationID
              dsp.getPathEdgeList().asScala.foreach{e =>
                val v1 = ctGraph.getEdgeSource(e)
                val v2 = ctGraph.getEdgeTarget(e)
                if (verts.last == v1) {
                  verts += v2
                } else {
                  verts += v1
                }
              }
              verts.toSet
            }
            
            
            val shortestPathVerts = client.getDomainShortestPath(leftJmolPalette.annotation.sampledConformationID, rightJmolPalette.annotation.sampledConformationID).filter(ctShortestPathVerts.contains(_))
            //            println("Got shortest path verts:  " + allShortestPathVerts.mkString(","))
            //            val shortestPathVerts = allShortestPathVerts.filter{v =>
            //              val coords = floorplan.contourTreeCriticalNodeToTerrainVerts.get(v) match {
            //                case Some(vertIDs) => vertIDs.map(floorplan.terrainVertices)
            //                case None => {
            //                  floorplan.contourTreeNoncriticalNodesToTerrainVerts.get(v) match {
            //                    case Some(vertList) => {
            //                      vertList.map(_.p)
            //                    }
            //                    case None => {scala.collection.mutable.ArrayBuffer.empty[Point3f]}
            //                  }
            //                }
            //              }
            //              
            //              coords.exists{c => 
            //                //pathVertsSet.contains(c)
            //                pathVerts.sliding(2).exists{pair =>
            //                  ptSegDistSq(pair(0), pair(1), c) < 1e-3
            //                }
            //              }
            //            }
            println("Filtered it down to:  " + shortestPathVerts.mkString(","))

            val sphereBranchGroup = new BranchGroup()
            sphereBranchGroup.setCapability(BranchGroup.ALLOW_DETACH)
            shortestPathVerts.foreach { v =>
              val closest = floorplan.contourTreeCriticalNodeToTerrainVerts.get(v) match {
                case Some(vertIDs) => {
                  // find vertex nearest to line array
                  val coords = vertIDs.map(floorplan.terrainVertices)
                  val closest = coords.minBy { c =>
                    pathVerts.map(v => v.distanceSquared(c)).min
                  }
                  closest
                }
                case None => {
                  floorplan.contourTreeNoncriticalNodesToTerrainVerts.get(v) match {
                    case Some(vertList) => {
                      val coords = vertList.map(_.p)
                      val closest = coords.minBy { c =>
                        pathVerts.map(v => v.distanceSquared(c)).min
                      }
                      closest
                    }
                    case None => new Point3f
                  }
                }
              }

              val t = new Transform3D()
              t.setTranslation(new Vector3f(closest))
              val tg = new TransformGroup(t)

              val appearance = new Appearance()
              appearance.setCapability(Appearance.ALLOW_COLORING_ATTRIBUTES_WRITE)
              val sphere = new Sphere(.004f)
              sphere.setAppearance(appearance)
              sphere.setPickable(true)
              sphere.getShape().setUserData(v)

              val colorAtts = new ColoringAttributes(new Color3f(Color.DARK_GRAY), ColoringAttributes.SHADE_GOURAUD);
              sphere.getAppearance().setColoringAttributes(colorAtts);

              val transAtts = new TransparencyAttributes(TransparencyAttributes.NICEST, .1f)
              sphere.getAppearance().setTransparencyAttributes(transAtts)

              tg.addChild(sphere);
              sphereBranchGroup.addChild(tg);
            }

            StoryboardPanel.this.publish(TerrainSpheresReady(sphereBranchGroup))
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

          }
        }
      } else if (e.getSource == menuCompareProteins) {
        val myIndex = storyboardEditor.contents.indexOf(DropArea.this)
        println("My index is:  " + myIndex)
        if (myIndex != 0 && myIndex < storyboardEditor.contents.size - 1) {
          val left = storyboardEditor.contents(myIndex - 1).asInstanceOf[AnnotationArea].annotation
          val right = storyboardEditor.contents(myIndex + 1).asInstanceOf[AnnotationArea].annotation

          val f = new Frame {
            title = "Compare conformations"
            preferredSize = new Dimension(800, 800)
            val jmolPanel = new JmolPanel
            contents = Component.wrap(jmolPanel)
            jmolPanel.compareConformations(left.pdbLines, right.pdbLines)
            visible = true
          }
        }
      }
    }

    reactions += {
      case e: MousePressed => {
        val myIndex = storyboardEditor.contents.indexOf(DropArea.this)
        println("My index is:  " + myIndex)
        if (myIndex != 0 && myIndex < storyboardEditor.contents.size - 1) {
          if (e.peer.isPopupTrigger()) {
            popup.show(e.peer.getComponent(),
              e.point.x, e.point.y);
          }
        }
      }
    }

    override def paintComponent(g2d: Graphics2D) = {
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2d.setColor(Color.DARK_GRAY)
      val rectInner = new RoundRectangle2D.Double(0, 0,
        bounds.width,
        bounds.height,
        10, 10)
      val rectOuter = new RoundRectangle2D.Double(0, 0,
        bounds.width - 1,
        bounds.height - 1,
        10, 10)
      g2d.fill(rectInner)
      g2d.setColor(scheme.lineColor)
      g2d.setStroke(dashedStroke)
      g2d.draw(rectOuter)
    }

    peer.setTransferHandler(new TransferHandler {
      override def canImport(support: TransferHandler.TransferSupport): Boolean = {
        return true
      }

      override def importData(support: TransferHandler.TransferSupport): Boolean = {
        if (!canImport(support))
          return false

        val t = support.getTransferable()
        println(t)
        val (annotation, move) = t.getTransferData(new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType)).asInstanceOf[Tuple2[ConformationAnnotation, Boolean]]
        val loc = support.getDropLocation()

        if (move) {
          println("p")
          val sourceIdx = storyboardEditor.contents.indexWhere { c => c.isInstanceOf[AnnotationArea] && c.asInstanceOf[AnnotationArea].annotation == annotation }
          println(sourceIdx)
          if (sourceIdx == storyboardEditor.contents.size - 1)
            return false
          storyboardEditor.contents.remove(sourceIdx, 2)
        }

        val myIndex = storyboardEditor.contents.indexOf(DropArea.this) match {
          case -1 => storyboardEditor.contents.size - 1
          case x: Int => x
        }
        println("My index is: " + myIndex)

        storyboardEditor.contents.insert(myIndex + 1, new DropArea)
        storyboardEditor.contents.insert(myIndex + 1, new AnnotationArea(annotation))

        storyboardEditor.revalidate()
        storyboardEditor.repaint()
        return true
      }

    })
  }

  class AnnotationArea(val annotation: ConformationAnnotation) extends Component {
    //    peer.setTransferHandler(new TransferHandler {
    //      override def getSourceActions(c: JComponent) = TransferHandler.COPY
    //      override def createTransferable(c: JComponent) = {
    //        val ret = new ConformationAnnotationTransferable(annotation, true)
    //        ret
    //      }
    //
    //    })

    focusable = true
    listenTo(mouse.clicks)
    listenTo(keys)
    listenTo(this)
    preferredSize = new Dimension(60, 100)
    minimumSize = new Dimension(60, 100)
    maximumSize = new Dimension(60, 100)
    override def paintComponent(g2d: Graphics2D) = {
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      val rect = new RoundRectangle2D.Double(0, 0,
        bounds.width,
        bounds.height,
        10, 10)

      g2d.setColor(scheme.btnBackground)
      g2d.fill(rect)
      if (hasFocus) {
        g2d.setColor(Color.orange)
        g2d.fill(rect)
      }

      if (annotation == controlPanel.activeAnnotation) {
        val innerRect = new RoundRectangle2D.Double(1, 1,
          bounds.width - 3,
          bounds.height - 3,
          10, 10)
        g2d.setColor(scheme.lineColor)
        g2d.setStroke(new BasicStroke(4))
        g2d.draw(innerRect)
      }

      g2d.setColor(Color.black)
      g2d.rotate(math.Pi / 2.0)
      g2d.drawString(annotation.name, 10, -25)
    }

    reactions += {
      case e: MousePressed => {
        requestFocus()
        //        storyboardEditor.contents.foreach{
        //          case area: AnnotationArea => area.selected = false
        //          case _ => {}
        //        }
        //        selected = true
        //        storyboardEditor.repaint()
        //        peer.getTransferHandler().exportAsDrag(peer, e.peer, TransferHandler.COPY)
      }
      case KeyPressed(_, key, _, _) => {
        if (key == Key.Delete || key == Key.BackSpace) {
          val idx = storyboardEditor.contents.indexOf(AnnotationArea.this)
          storyboardEditor.contents.remove(idx, 2)
          storyboardEditor.revalidate()
          storyboardEditor.repaint()
        }
      }
      case e: FocusGained => {
        repaint
      }
      case e: FocusLost => {
        repaint
      }
    }

  }

}

object StoryboardPanel {
  val dashedStroke =
    new BasicStroke(1.0f,
      BasicStroke.CAP_BUTT,
      BasicStroke.JOIN_MITER,
      6.0f, Array(6.0f), 0.0f);
  val stopIcon = {
    val img = GraphicsUtilities.createCompatibleImage(17, 17)
    val g2d = img.createGraphics()
    g2d.setColor(scheme.btnBackground)
    g2d.fillRect(0, 0, img.getWidth, img.getHeight)
    g2d.setColor(scheme.btnForeground)
    g2d.drawRect(0, 0, img.getWidth - 1, img.getHeight - 1)
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    val gp = new GeneralPath
    gp.moveTo(4, 4)
    gp.lineTo(4, 12)
    gp.lineTo(11, 12)
    gp.lineTo(11, 4)
    gp.closePath()
    g2d.fill(gp)
    g2d.setColor(scheme.lineColor)
    g2d.draw(gp)
    new ImageIcon(img)
  }

  val fwdIcon = {
    val img = GraphicsUtilities.createCompatibleImage(17, 17)
    val g2d = img.createGraphics()
    g2d.setColor(scheme.btnBackground)
    g2d.fillRect(0, 0, img.getWidth, img.getHeight)
    g2d.setColor(scheme.btnForeground)
    g2d.drawRect(0, 0, img.getWidth - 1, img.getHeight - 1)
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    val gp = new GeneralPath
    gp.moveTo(3, 3)
    gp.lineTo(3, 13)
    gp.lineTo(10, 8)
    gp.closePath()
    gp.moveTo(12, 3)
    gp.lineTo(12, 13)
    gp.lineTo(13, 13)
    gp.lineTo(13, 3)
    gp.closePath()
    g2d.fill(gp)
    g2d.setColor(scheme.lineColor)
    g2d.draw(gp)
    new ImageIcon(img)
  }

  val backIcon = {
    val img = GraphicsUtilities.createCompatibleImage(17, 17)
    val g2d = img.createGraphics()
    g2d.setColor(scheme.btnBackground)
    g2d.fillRect(0, 0, img.getWidth, img.getHeight)
    g2d.setColor(scheme.btnForeground)
    g2d.drawRect(0, 0, img.getWidth - 1, img.getHeight - 1)
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    val gp = new GeneralPath
    gp.moveTo(3, 3)
    gp.lineTo(3, 13)
    gp.lineTo(4, 13)
    gp.lineTo(4, 3)
    gp.closePath()
    gp.moveTo(6, 8)
    gp.lineTo(13, 3)
    gp.lineTo(13, 13)
    gp.closePath
    gp.closePath()
    g2d.fill(gp)
    g2d.setColor(scheme.lineColor)
    g2d.draw(gp)
    new ImageIcon(img)
  }
}
