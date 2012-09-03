package edu.osu.compgeom.omegavis

import edu.osu.compgeom.omegavis.desktop.DesktopPane
import scala.swing._
import scala.swing.event._
import javax.swing.OverlayLayout
import edu.osu.compgeom.omegavis.state.StateManager
import javax.swing.JLayeredPane
import edu.osu.compgeom.omegavis.event.RingMenuUpdate
import edu.osu.compgeom.omegavis.event.ContourTreeReady
import edu.osu.compgeom.omegavis.event.ColormapUpdate
import javax.media.j3d.PointArray
import edu.osu.compgeom.omegavis.event.SetCameraEvent
import javax.media.j3d.Shape3D
import edu.osu.compgeom.topology.ScalarFunction
import javax.media.j3d.GeometryArray
import javax.vecmath.Point3f
import javax.media.j3d.Appearance
import javax.media.j3d.Canvas3D
import javax.media.j3d.PointAttributes
import java.awt.GraphicsEnvironment
import javax.media.j3d.GraphicsConfigTemplate3D
import javax.vecmath.Point3d
import java.awt.GraphicsConfigTemplate
import javax.vecmath.Point2d
import javax.media.j3d.Transform3D
import java.awt.RenderingHints
import java.awt.Color
import java.awt.BasicStroke
import javax.vecmath.Vector3d
import org.jdesktop.swingx.MultiSplitPane
import org.jdesktop.swingx.MultiSplitLayout.Divider
import org.jdesktop.swingx.MultiSplitLayout.Leaf
import org.jdesktop.swingx.MultiSplitLayout.Split
import java.util.Arrays
import org.jdesktop.swingx.MultiSplitPane.DividerPainter
import java.awt.Graphics
import edu.osu.compgeom.omegavis.event.SelectionUpdated
import edu.osu.compgeom.ct.ContourTreeEdge
import javax.vecmath.Color4f
import edu.osu.compgeom.ct.ContourTree
import javax.media.j3d.BranchGroup
import edu.osu.compgeom.omegavis.event.PointCloudViewUpdated

//class QuadFrame(stateManager: StateManager) extends Frame {
//  var ringMenu: RingMenu = null
//  var hud: GammaHUD = null
//
//  title = "Dataset Explorer"
//
//  size = new Dimension(1600, 800)
//
//  listenTo(this)
//
//  reactions += {
//    case e: WindowClosing => {
//      println("Window closing.")
//      pointCloudView.simpleUniverse.cleanup()
//      contourTreePanel.simpleUniverse.cleanup()
//      terrainPanel.simpleUniverse.cleanup()
//    }
//  }
//
//  val pointCloudView = new MutatorPointCloudView {
//    setPreferredSize(new Dimension(500, 500))
//    this.listenTo(this)
//
//    var cmap = GammaColormaps.jet(_)
//    var pointArray: PointArray = null
//
//    reactions += {
//      case SetCameraEvent(rotationCenter, zoom) => {
//
//        // Need to transform rotation center
//        val t3D = new Transform3D
//        masterTransformGroup.getTransform(t3D)
//        t3D.transform(rotationCenter)
//        pickingOrbitBehavior.setRotationCenter(rotationCenter)
//
//        // New code
//        val viewTrans = new Transform3D
//        viewTrans.setIdentity
//        viewTrans.lookAt(new Point3d(rotationCenter.x, rotationCenter.y, 1), rotationCenter, new Vector3d(0, 1, 0))
//        viewTrans.invert
//        simpleUniverse.getViewingPlatform().getViewPlatformTransform().setTransform(viewTrans)
//        repaint
//      }
//
//      case SelectionUpdated(selectedEdges) => {
//        val pointToContourTreeEdge = new Array[ContourTreeEdge](stateManager.morseFunction.vertices.size)
//        stateManager.ctSimp.criticalNodeToIncidentEdges.values.flatten.foreach(ctEdge => {
//          pointToContourTreeEdge(ctEdge.n1.vertex) = ctEdge
//          pointToContourTreeEdge(ctEdge.n2.vertex) = ctEdge
//          ctEdge.noncriticalNodes.foreach(n => { pointToContourTreeEdge(n.vertex) = ctEdge })
//        })
//
//        val sf = stateManager.morseFunction
//        def getNodeColor(f: Float, ctEdge: ContourTreeEdge) = {
//          if (selectedEdges.contains(ctEdge)) {
//            new Color4f(Color.yellow)
//          } else {
//            val h1 = sf.getFuncVal(ctEdge.n1.vertex)
//            val h2 = sf.getFuncVal(ctEdge.n2.vertex)
//            val hMin = math.min(h1, h2)
//            val hMax = math.max(h1, h2)
//            val alpha = (f - h2) / (h1 - h2)
//            cmap(sf)(ctEdge, alpha)
//          }
//        }
//
//        val colors = stateManager.morseFunction.vertices.indices.map(i => {
//          getNodeColor(sf.getFuncVal(i), pointToContourTreeEdge(i))
//        }).toArray
//        println(colors.size)
//
//        val colorIndices = stateManager.morseFunction.faces.flatten
//
//        pointArray.setColors(0, colors)
//      }
//
//      case ContourTreeReady(ct: ContourTree) => {
//        val sf = ct.scalarFunction
//        val newWorldRoot = new BranchGroup
//        val extrema = (for (i <- 0 until 3) yield {
//          val c = sf.vertices.map(_(i))
//          (c.min, c.max)
//        }).toArray
//
//        val minFuncVal = sf.vertices.indices.map(sf.getFuncVal).min
//        val maxFuncVal = sf.vertices.indices.map(sf.getFuncVal).max
//        val scale = 1.7f / (extrema.map(_._2).max - extrema.map(_._1).min)
//        setViewScale(scale)
//
//        newWorldRoot.addChild(getColoredPointCloud(sf, scale))
//        updateWorld(newWorldRoot)
//
//        publish(new PointCloudViewUpdated(pointArray, ct))
//      }
//    }
//
//    def getColoredPointCloud(sf: ScalarFunction, scale: Float): Shape3D = {
//      pointArray = new PointArray(sf.vertices.size, GeometryArray.COORDINATES | GeometryArray.COLOR_4)
//      pointArray.setCapability(GeometryArray.ALLOW_COLOR_WRITE)
//      pointArray.setCoordinates(0, sf.vertices.map(v => new Point3f(v(0), v(1), v(2))))
//      val appearance = new Appearance
//      appearance.setPointAttributes(new PointAttributes(4, true))
//
//      val s3d = new Shape3D(pointArray, appearance)
//      return s3d
//    }
//  }
//
//  import java.awt.geom._
//
//  val canvas3D = new Canvas3D(GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getBestConfiguration {
//    new GraphicsConfigTemplate3D {
//      setSceneAntialiasing(GraphicsConfigTemplate.PREFERRED)
//    }
//  }) {
//
//    override def postRender(): Unit = {
//      tcManager.renderHud(this)
//
//      def get3dTo2dPoint(point3d: Point3d): Point2d = {
//        val temp = new Transform3D();
//        getVworldToImagePlate(temp);
//        temp.transform(point3d);
//        val point2d = new Point2d();
//        getPixelLocationFromImagePlate(point3d, point2d);
//        return point2d;
//      }
//
//      val g2d = getGraphics2D()
//      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
//      g2d.setColor(Color.black)
//
//      g2d.setStroke(new BasicStroke(4))
//      val r = 6
//
//      val p1 = get3dTo2dPoint(new Point3d(-0.08756825, 0.30226624, 0.0))
//      val p2 = get3dTo2dPoint(new Point3d(-0.11385262, 0.29914856, 0.028496))
//      val p3 = get3dTo2dPoint(new Point3d(-0.107916296, 0.3163203, 0.061643112))
//      val p4 = get3dTo2dPoint(new Point3d(-0.094905615, 0.31516647, 0.03074884))
//      val p5 = get3dTo2dPoint(new Point3d(-0.115135014, 0.30590665, 0.045168117))
//
//      g2d.flush(true)
//    }
//  }
//
//  val terrainPanel = new TerrainPanel(canvas3D) {
//    preferredSize = new Dimension(500, 500)
//
//    reactions += {
//      case ContourTreeReady(ct) => {
////        val nodeAtInfinity = ct.nodesContracted.foldLeft(ct.nodesContracted(0))((x, y) => {
////          if (ct.scalarFunction.getFuncVal(x.vertex) < ct.scalarFunction.getFuncVal(y.vertex)) y else x
////        })
//
//        publish(new RegenerateTerrainEvent(ct, stateManager.nodeAtInfinity))
//      }
//    }
//  }
//
//  val contourTreePanel = new ContourTreePanel(new Canvas3D(GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getBestConfiguration {
//    new GraphicsConfigTemplate3D {
//      setSceneAntialiasing(GraphicsConfigTemplate.PREFERRED)
//    }
//  })) {
//    preferredSize = new Dimension(300, 500)
//  }
//
//  val tcManager: TopologicalComponentManager = new TopologicalComponentManager(contourTreePanel, pointCloudView, terrainPanel)
//
//  val conformationalSpacePanel = new ConformationalSpacePanel
//  conformationalSpacePanel.pointsPanel.listenTo(tcManager)
//  conformationalSpacePanel.ssPanel.listenTo(tcManager)
//
//  val multiSplitPane = new MultiSplitPane
//  val div_1_2 = new Divider
//  val div_2_3 = new Divider
//  val div_3_4 = new Divider
//
//  val layeredPane = peer.getRootPane.getLayeredPane
//  layeredPane.setLayout(new OverlayLayout(layeredPane))
//
//  contourTreePanel.listenTo(terrainPanel)
//  pointCloudView.listenTo(terrainPanel)
//
//  tcManager.listenTo(terrainPanel)
//  tcManager.listenTo(contourTreePanel)
//  tcManager.listenTo(pointCloudView)
//  tcManager.listenTo(conformationalSpacePanel.pointsPanel)
//
//  ringMenu = new RingMenu(this) { visible = false }
//  stateManager.listenTo(ringMenu)
//
//  terrainPanel.listenTo(stateManager)
//  terrainPanel.listenTo(tcManager)
//  contourTreePanel.listenTo(stateManager)
//  pointCloudView.listenTo(stateManager)
//  tcManager.listenTo(stateManager)
//
//  ringMenu.reactions(new RingMenuUpdate(stateManager.awaitTauOrColorFuncMenu))
//
//  ringMenu.listenTo(stateManager)
//
//  layeredPane.add(ringMenu.peer, JLayeredPane.MODAL_LAYER)
//
//  def ui = {
//    val col1 = new Leaf("col1")
//    col1.setWeight(1.0 / 4.0)
//    val col2 = new Leaf("col2")
//    col2.setWeight(1.0 / 4.0)
//    val col3 = new Leaf("col3")
//    col3.setWeight(1.0 / 4.0)
//    val col4 = new Leaf("col4")
//    col4.setWeight(1.0 / 4.0)
//
//    val modelRoot = new Split
//    modelRoot.setChildren(Arrays.asList(col1, div_1_2, col2, div_2_3, col3, div_3_4, col4))
//    modelRoot.setRowLayout(true)
//
//    multiSplitPane.setBackground(Color.black)
//    multiSplitPane.getMultiSplitLayout().setModel(modelRoot)
//    multiSplitPane.setDividerPainter(new DividerPainter {
//      def paint(g: Graphics, div: Divider): Unit = {
//        val g2d = g.asInstanceOf[Graphics2D]
//        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
//
//        val bounds = div.getBounds
//
//        g2d.setColor(Color.black)
//        g2d.fillRect(bounds.x, bounds.y, bounds.width, bounds.height)
//
//        g2d.setColor(new Color(73, 96, 112))
//        val x = bounds.x + bounds.width / 2
//        g2d.drawLine(x, bounds.y + 3, x, bounds.y + bounds.height - 6)
//        g2d.fillRect(bounds.x + 3, bounds.y + 3, bounds.width - 6, bounds.height - 6)
//        g2d.setColor(new Color(73, 96, 112))
//        g2d.drawRect(bounds.x + 3, bounds.y + 3, bounds.width - 6, bounds.height - 6)
//      }
//    })
//
//    multiSplitPane.add(pointCloudView, "col1")
//    multiSplitPane.add(contourTreePanel.peer, "col2");
//    multiSplitPane.add(conformationalSpacePanel.peer, "col3")
//
//    val desktopPane = new DesktopPane(terrainPanel)
//    desktopPane.peer.setPreferredSize(new Dimension(900, 900))
//    desktopPane.revalidate()
//    desktopPane.listenTo(conformationalSpacePanel.pointsPanel)
//    tcManager.listenTo(desktopPane)
//
//    multiSplitPane.add(desktopPane.peer, "col4");
//
//    multiSplitPane.setDividerSize(10)
//
//    val bp = new BorderPanel {
//      add((new Button("Menu") {
//        foreground = new Color(177, 186, 217)
//        background = new Color(10, 10, 10)
//        reactions += {
//          case e: ButtonClicked => if (!ringMenu.visible) ringMenu.visible = true
//        }
//      }), BorderPanel.Position.North);
//
//      add(Component.wrap(multiSplitPane), BorderPanel.Position.Center)
//    }
//    bp.focusable = true
//    listenTo(bp.keys)
//    bp
//  }
//
//  contents = ui
//
//  stateManager.tcManager = tcManager
//  stateManager.terrainPanel = terrainPanel
//  stateManager.contourTreePanel = contourTreePanel
//  stateManager.pointCloudView = pointCloudView
//  conformationalSpacePanel.stateManager = stateManager
//
//  stateManager.publish(new ContourTreeReady(stateManager.ctSimp))
////  publish(new RegenerateTerrainEvent(stateManager.ctSimp, stateManager.nodeAtInfinity))
//  stateManager.publish(new ColormapUpdate(stateManager.colormapGenerator(stateManager.colorFunction), stateManager.colorFunction))
//
//}