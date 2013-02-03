/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Graphics
import java.awt.GraphicsConfigTemplate
import java.awt.GraphicsEnvironment
import java.awt.RenderingHints
import java.io.BufferedReader
import java.io.File
import java.io.FileReader
import java.util.Arrays
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.annotation.tailrec
import scala.swing.event.ButtonClicked
import scala.swing.Dimension
import scala.swing.Graphics2D
import scala.swing.BorderPanel
import scala.swing.Button
import scala.swing.Component
import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication
import org.jdesktop.swingx.MultiSplitLayout.Divider
import org.jdesktop.swingx.MultiSplitLayout.Leaf
import org.jdesktop.swingx.MultiSplitLayout.Split
import org.jdesktop.swingx.MultiSplitPane.DividerPainter
import org.jdesktop.swingx.MultiSplitPane
import ayla.geometry.ct.ContourTree
import ayla.geometry.ct.ContourTreeEdge
import ayla.dataset.CachedDataset
import ayla.client.ui.desktop.DesktopPane
import ayla.client.ui.event.ColormapUpdate
import ayla.client.ui.event.ContourTreeReady
import ayla.client.ui.event.PointCloudViewUpdated
import ayla.client.ui.event.RingMenuUpdate
import ayla.client.ui.event.SelectionUpdated
import ayla.client.ui.event.SetCameraEvent
import ayla.client.ui.event.TriangulatedPointCloudViewUpdated
import ayla.client.ui.menu._
import ayla.geometry.ScalarFunction
import javax.media.j3d.Appearance
import javax.media.j3d.BranchGroup
import javax.media.j3d.Canvas3D
import javax.media.j3d.GeometryArray
import javax.media.j3d.GraphicsConfigTemplate3D
import javax.media.j3d.PointArray
import javax.media.j3d.PointAttributes
import javax.media.j3d.Shape3D
import javax.media.j3d.IndexedTriangleArray
import javax.media.j3d.Transform3D
import javax.swing.JLayeredPane
import javax.swing.OverlayLayout
import javax.swing.UIManager
import javax.vecmath.Color4f
import javax.vecmath.Point2d
import javax.vecmath.Point3d
import javax.vecmath.Point3f
import javax.vecmath.Vector3d
import scala.io.Source
import ayla.util.Timing
import scala.sys.process.Process
import ayla.util.IO._
import javax.vecmath.Color3f
import java.awt.BorderLayout
import javax.media.j3d.ColoringAttributes
import javax.media.j3d.PolygonAttributes
import javax.media.j3d.Material
import javax.media.j3d.AmbientLight
import ayla.server.CollaborationProject
import scala.collection.mutable.SynchronizedSet
import ayla.geometry.ct.ContourTreeNode
import scala.swing.Frame
import ayla.client.AylaClientCachedDataset
import ayla.colormap._

object DatasetExplorer extends SimpleSwingApplication {
  var stateManager: StateManager = null

  def launch(ct: ContourTree, nodeAtInfinity: ContourTreeNode, dataset: AylaClientCachedDataset): Unit = {
//    HexProgressMonitor2.runTasks("Launching Dataset Explorer") { hpMon =>
//
//      hpMon.add2("initializing state manager") {
//        stateManager = new StateManager(dataset, ct.scalarFunction, ct, nodeAtInfinity, conformationalSpacePanel)
//        stateManager.tcManager = tcManager
//        stateManager.terrainPanel = terrainPanel
//        stateManager.contourTreePanel = contourTreePanel
//        stateManager.pointCloudView = pointCloudView
//
//        conformationalSpacePanel.stateManager = stateManager
//        conformationalSpacePanel.dataset = dataset
//        desktopPane.listenTo(dataset.client)
//
//        dataset.client.collabFrame.storyboardPanel.desktopPane = desktopPane
//
//        terrainPanel.listenTo(dataset.client.collabFrame.storyboardPanel)
//        desktopPane.listenTo(dataset.client.collabFrame)
//        dataset.client.collabFrame.storyboardPanel.listenTo(terrainPanel)
//        desktopPane.listenTo(terrainPanel)
//        dataset.client.collabFrame.visible = true
//      }
//
//      hpMon.add2("Wiff") {
//        dataset.client.refreshAnnotationList()
//      }
//
//      hpMon.add2("Waff") {
//        dataset.client.refreshStoryboardList()
//      }
//    }
    
        stateManager = new StateManager(dataset, ct.scalarFunction, ct, nodeAtInfinity, conformationalSpacePanel)
        stateManager.tcManager = tcManager
        stateManager.terrainPanel = terrainPanel
        stateManager.contourTreePanel = contourTreePanel
        stateManager.pointCloudView = pointCloudView

        conformationalSpacePanel.stateManager = stateManager
        conformationalSpacePanel.dataset = dataset
        desktopPane.listenTo(dataset.client)

        dataset.client.collabFrame.storyboardPanel.desktopPane = desktopPane

        terrainPanel.listenTo(dataset.client.collabFrame.storyboardPanel)
        desktopPane.listenTo(dataset.client.collabFrame)
        dataset.client.collabFrame.storyboardPanel.listenTo(terrainPanel)
        desktopPane.listenTo(terrainPanel)
        dataset.client.collabFrame.visible = true
        
        

    super.main(Array.empty[String])
  }

  var ringMenu: RingMenu = null

  def top = new MainFrame {
    title = "Dataset Explorer"

    contents = ui
    size = new Dimension(1600, 800)

    val layeredPane = peer.getRootPane.getLayeredPane
    layeredPane.setLayout(new OverlayLayout(layeredPane))

    contourTreePanel.listenTo(terrainPanel)
    pointCloudView.listenTo(terrainPanel)

    tcManager.listenTo(terrainPanel)
    tcManager.listenTo(contourTreePanel)
    tcManager.listenTo(pointCloudView)
    tcManager.listenTo(conformationalSpacePanel.pointsPanel)

    ringMenu = new RingMenu(this) { visible = false }
    stateManager.listenTo(ringMenu)

    terrainPanel.listenTo(stateManager)
    terrainPanel.listenTo(tcManager)
    contourTreePanel.listenTo(stateManager)
    pointCloudView.listenTo(stateManager)
    tcManager.listenTo(stateManager)

    ringMenu.reactions(new RingMenuUpdate(stateManager.awaitTauOrColorFuncMenu))

    ringMenu.listenTo(stateManager)

    layeredPane.add(ringMenu.peer, JLayeredPane.MODAL_LAYER)

    stateManager.publish(new ContourTreeReady(stateManager.ctSimp))
    stateManager.publish(new ColormapUpdate(stateManager.colormapGenerator(stateManager.colorFunction), stateManager.colorFunction))
  }

  val pointCloudView = new MutatorPointCloudView {
    preferredSize = new Dimension(500, 500)
    this.listenTo(this)

    var cmap = AylaColormaps.jet(_)
    var pointArray: PointArray = null

    reactions += {
      case SetCameraEvent(rotationCenter, zoom) => {

        // Need to transform rotation center
        val t3D = new Transform3D
        masterTransformGroup.getTransform(t3D)
        t3D.transform(rotationCenter)
        pickingOrbitBehavior.setRotationCenter(rotationCenter)

        val viewTrans = new Transform3D
        viewTrans.setIdentity
        viewTrans.lookAt(new Point3d(rotationCenter.x, rotationCenter.y, 1), rotationCenter, new Vector3d(0, 1, 0))
        viewTrans.invert
        simpleUniverse.getViewingPlatform().getViewPlatformTransform().setTransform(viewTrans)
        repaint
      }

      case SelectionUpdated(selectedEdges) => {
        val pointToContourTreeEdge = new Array[ContourTreeEdge](stateManager.morseFunction.vertices.size)
        stateManager.ctSimp.criticalNodeToIncidentEdges.values.flatten.foreach(ctEdge => {
          pointToContourTreeEdge(ctEdge.n1.vertex) = ctEdge
          pointToContourTreeEdge(ctEdge.n2.vertex) = ctEdge
          ctEdge.noncriticalNodes.foreach(n => { pointToContourTreeEdge(n.vertex) = ctEdge })
        })

        val sf = stateManager.morseFunction
        def getNodeColor(f: Float, ctEdge: ContourTreeEdge) = {
          if (selectedEdges.contains(ctEdge)) {
            new Color4f(Color.yellow)
          } else {
            val h1 = sf.getFuncVal(ctEdge.n1.vertex)
            val h2 = sf.getFuncVal(ctEdge.n2.vertex)
            val hMin = math.min(h1, h2)
            val hMax = math.max(h1, h2)
            val alpha = (f - h2) / (h1 - h2)
            cmap(sf)(ctEdge, alpha)
          }
        }

        val colors = stateManager.morseFunction.vertices.indices.map(i => {
          getNodeColor(sf.getFuncVal(i), pointToContourTreeEdge(i))
        }).toArray
        println(colors.size)

        val colorIndices = stateManager.morseFunction.faces.flatten

        pointArray.setColors(0, colors)
      }

      case ContourTreeReady(ct: ContourTree) => {
        val sf = ct.scalarFunction
        val newWorldRoot = new BranchGroup
        val extrema = (for (i <- 0 until 3) yield {
          val c = sf.vertices.map(_(i))
          (c.min, c.max)
        }).toArray

        val minFuncVal = sf.vertices.indices.map(sf.getFuncVal).min
        val maxFuncVal = sf.vertices.indices.map(sf.getFuncVal).max
        val scale = 1.7f / (extrema.map(_._2).max - extrema.map(_._1).min)
        setViewScale(scale)

        newWorldRoot.addChild(getColoredPointCloud(sf, scale))
        updateWorld(newWorldRoot)

        publish(new PointCloudViewUpdated(pointArray, ct))
      }
    }

    def getColoredPointCloud(sf: ScalarFunction, scale: Float): Shape3D = {
      pointArray = new PointArray(sf.vertices.size, GeometryArray.COORDINATES | GeometryArray.COLOR_4)
      pointArray.setCapability(GeometryArray.ALLOW_COLOR_WRITE)
      pointArray.setCoordinates(0, sf.vertices.map(v => new Point3f(v(0), v(1), v(2))))
      val appearance = new Appearance
      appearance.setPointAttributes(new PointAttributes(4, true))

      val s3d = new Shape3D(pointArray, appearance)
      return s3d
    }
  }

  val terrainPanel = new TerrainPanel(tcManager.renderHud(_)) {
    preferredSize = new Dimension(500, 500)

    reactions += {
      case ContourTreeReady(ct) => {
        val minFuncVal = ct.scalarFunction.vertices.indices.map(ct.scalarFunction.getFuncVal).min
        val maxFuncVal = ct.scalarFunction.vertices.indices.map(ct.scalarFunction.getFuncVal).max
        val nodeAtInfinity = ct.nodesContracted.foldLeft(ct.nodesContracted(0))((x, y) => {
          if (ct.scalarFunction.getFuncVal(x.vertex) < ct.scalarFunction.getFuncVal(y.vertex)) y else x
        })

        publish(new RegenerateTerrainEvent(ct, nodeAtInfinity))
      }
    }
  }
  
  val desktopPane = new DesktopPane(terrainPanel)

  val contourTreePanel = new ContourTreePanel(new Canvas3D(GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getBestConfiguration {
    new GraphicsConfigTemplate3D {
      setSceneAntialiasing(GraphicsConfigTemplate.PREFERRED)
    }
  })) {
    preferredSize = new Dimension(300, 500)
  }

  val tcManager: TopologicalComponentManager = new TopologicalComponentManager(contourTreePanel, pointCloudView, terrainPanel)
  terrainPanel.tcManager = tcManager

  val conformationalSpacePanel = new ConformationalSpacePanel
  conformationalSpacePanel.pointsPanel.listenTo(tcManager)
  conformationalSpacePanel.pointsPanel.listenTo(terrainPanel)
  conformationalSpacePanel.ssPanel.listenTo(tcManager)

  val multiSplitPane = new MultiSplitPane
  val div_3_4 = new Divider

  val ui = {
    val col1 = new Leaf("col1")
    col1.setWeight(1.0 / 4.0)
    val col2 = new Leaf("col2")
    col2.setWeight(1.0 / 4.0)
    val col3 = new Leaf("col3")
    col3.setWeight(1.0 / 4.0)
    val col4 = new Leaf("col4")
    col4.setWeight(1.0 / 4.0)

    val modelRoot = new Split
    modelRoot.setChildren(Arrays.asList(col3, div_3_4, col4))
    modelRoot.setRowLayout(true)

    multiSplitPane.setBackground(ColorSchemes.scheme.bgColor)
    multiSplitPane.getMultiSplitLayout().setModel(modelRoot)
    multiSplitPane.setDividerPainter(new DividerPainter {
      def paint(g: Graphics, div: Divider): Unit = {
        val g2d = g.asInstanceOf[Graphics2D]
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

        val bounds = div.getBounds

        g2d.setColor(ColorSchemes.scheme.bgColor)
        g2d.fillRect(bounds.x, bounds.y, bounds.width, bounds.height)

        g2d.setColor(ColorSchemes.scheme.lineColor)
        val x = bounds.x + bounds.width / 2
        g2d.drawLine(x, bounds.y + 3, x, bounds.y + bounds.height - 6)
        g2d.fillRect(bounds.x + 3, bounds.y + 3, bounds.width - 6, bounds.height - 6)
        g2d.setColor(ColorSchemes.scheme.lineColor)
        g2d.drawRect(bounds.x + 3, bounds.y + 3, bounds.width - 6, bounds.height - 6)
      }
    })

    multiSplitPane.add(conformationalSpacePanel.peer, "col3")

    desktopPane.listenTo(conformationalSpacePanel.pointsPanel)
    tcManager.listenTo(desktopPane)

    multiSplitPane.add(desktopPane.peer, "col4");

    multiSplitPane.setDividerSize(10)

    val bp = new BorderPanel {
      add((new Button("Menu") {
        foreground = ColorSchemes.scheme.btnForeground
        background = ColorSchemes.scheme.btnBackground
        reactions += {
          case e: ButtonClicked => if (!ringMenu.visible) ringMenu.visible = true
        }
      }), BorderPanel.Position.North);

      add(Component.wrap(multiSplitPane), BorderPanel.Position.Center)

    }
    bp.focusable = true
    listenTo(bp.keys)
    bp
  }

}
