package edu.osu.compgeom.omegavis

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
import edu.osu.compgeom.ct.ContourTree
import edu.osu.compgeom.ct.ContourTreeEdge
import edu.osu.compgeom.dataset.CachedDataset
import edu.osu.compgeom.omegavis.desktop.DesktopPane
import edu.osu.compgeom.omegavis.event.ColormapUpdate
import edu.osu.compgeom.omegavis.event.ContourTreeReady
import edu.osu.compgeom.omegavis.event.PointCloudViewUpdated
import edu.osu.compgeom.omegavis.event.RingMenuUpdate
import edu.osu.compgeom.omegavis.event.SelectionUpdated
import edu.osu.compgeom.omegavis.event.SetCameraEvent
import edu.osu.compgeom.omegavis.event.TriangulatedPointCloudViewUpdated
import edu.osu.compgeom.omegavis.state.StateManager
import edu.osu.compgeom.topology.ScalarFunction
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
import scopt.OptionParser
import scala.io.Source
import edu.osu.compgeom.util.Timing
import scala.sys.process.Process
import edu.osu.compgeom.util.IO._
import javax.vecmath.Color3f
import java.awt.BorderLayout
import javax.media.j3d.ColoringAttributes
import javax.media.j3d.PolygonAttributes
import javax.media.j3d.Material
import javax.media.j3d.AmbientLight
import edu.osu.compgeom.ayla.AylaCollaborationProject
import edu.osu.compgeom.dataset.preprocess.KnnDatabase
import scala.collection.mutable.SynchronizedSet
import edu.osu.compgeom.ct.ContourTreeNode
import edu.osu.compgeom.ayla._
import edu.osu.compgeom.ayla.message._
import scala.swing.Frame
import akka.actor.ActorSystem

object DatasetExplorer extends SimpleSwingApplication {
  var datasetsRootDir: File = null
  var stateManager: StateManager = null
  var desktopPane: DesktopPane = null

  def launch(ct: ContourTree, nodeAtInfinity: ContourTreeNode, dataset: AylaClientCachedDataset): Unit = {
    HexProgressMonitor2.runTasks("Launching Dataset Explorer") {pt =>
      pt.add2("initializing state manager and collaboration frame") {
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
	      scala.swing.Swing.onEDT(dataset.client.collabFrame.visible = true)
      }
      
      pt.add2("retrieving annotation list from server") {
        dataset.client.clientActor ! RefreshAnnotationListRequest
      }
      
      pt.add2("retrieving storyboards from server") {
        dataset.client.clientActor ! RefreshStoryboardListRequest
      }
    }
//      stateManager = new StateManager(dataset, ct.scalarFunction, ct, nodeAtInfinity, conformationalSpacePanel)
//      stateManager.tcManager = tcManager
//      stateManager.terrainPanel = terrainPanel
//      stateManager.contourTreePanel = contourTreePanel
//      stateManager.pointCloudView = pointCloudView

//      conformationalSpacePanel.stateManager = stateManager
//      conformationalSpacePanel.dataset = dataset
//      desktopPane.listenTo(dataset.client)
//      
//      dataset.client.collabFrame.storyboardPanel.desktopPane = desktopPane
//      
//      terrainPanel.listenTo(dataset.client.collabFrame.storyboardPanel)
//      desktopPane.listenTo(dataset.client.collabFrame)
//      dataset.client.collabFrame.storyboardPanel.listenTo(terrainPanel)
//      desktopPane.listenTo(terrainPanel)
//      dataset.client.collabFrame.visible = true
      
//      dataset.client.clientActor ! RefreshAnnotationListRequest
//      
//      dataset.client.clientActor ! RefreshStoryboardListRequest

    super.main(Array.empty[String])
  }

  var ringMenu: RingMenu = null
  var hud: GammaHUD = null

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
    /*
    triangulatedPointCloudView.listenTo(stateManager)
    */
    tcManager.listenTo(stateManager)

    ringMenu.reactions(new RingMenuUpdate(stateManager.awaitTauOrColorFuncMenu))

    ringMenu.listenTo(stateManager)

    layeredPane.add(ringMenu.peer, JLayeredPane.MODAL_LAYER)
    //		ringMenuHandler.listenTo(ringMenu)

    stateManager.publish(new ContourTreeReady(stateManager.ctSimp))
    stateManager.publish(new ColormapUpdate(stateManager.colormapGenerator(stateManager.colorFunction), stateManager.colorFunction))
    //		hud = new GammaHUD
    //		layeredPane.add(hud.peer, JLayeredPane.PALETTE_LAYER)
  }

  val pointCloudView = new MutatorPointCloudView {
    preferredSize = new Dimension(500, 500)
    this.listenTo(this)

    var cmap = GammaColormaps.jet(_)
    //		var morseCrystalLineArray: IndexedLineArray = null
    var pointArray: PointArray = null

    reactions += {
      case SetCameraEvent(rotationCenter, zoom) => {

        // Need to transform rotation center
        val t3D = new Transform3D
        masterTransformGroup.getTransform(t3D)
        t3D.transform(rotationCenter)
        pickingOrbitBehavior.setRotationCenter(rotationCenter)
        //			orbit.setZoomFactor(.01)

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

  /*
  val triangulatedPointCloudView = new MutatorPointCloudView {
    setPreferredSize(new Dimension(500, 500))
    this.listenTo(this)
    
    var cmap = GammaColormaps.jet(_)
    var indexedTriArray: IndexedTriangleArray = null
    //    var pointArray: PointArray = null

    reactions += {
      case SetCameraEvent(rotationCenter, zoom) => {

        // Need to transform rotation center
        val t3D = new Transform3D
        masterTransformGroup.getTransform(t3D)
        t3D.transform(rotationCenter)
        pickingOrbitBehavior.setRotationCenter(rotationCenter)
        //			orbit.setZoomFactor(.01)

        val viewTrans = new Transform3D
        viewTrans.setIdentity
        viewTrans.lookAt(new Point3d(rotationCenter.x, rotationCenter.y, 1), rotationCenter, new Vector3d(0, 1, 0))
        viewTrans.invert
        simpleUniverse.getViewingPlatform().getViewPlatformTransform().setTransform(viewTrans)
        repaint
      }
    }

    def setPoints(pts: Array[Array[Float]], sf: ScalarFunction): Unit = {
      val tmpDir = new File("/dev/shm/")
      val prefix = "verts"
      val nodeFile = new File(tmpDir, "%s.node".format(prefix))
      withBufferedWriter(nodeFile) { bw =>
        bw.write("%d 2 1 0\n".format(pts.size))
        pts.iterator.zipWithIndex.foreach {
          case (p, i) =>
            bw.write(i + p.mkString(" ", " ", "\n"))
        }
      }

      val triangleCommand = "/home/harveywi/Desktop/triangle/triangle " + nodeFile.getName
      val pb = Process(triangleCommand, tmpDir)
      pb.!

      nodeFile.delete()

      // Read triangles
      val outFiles = List("%s.1.ele", "%s.1.node", "%s.1.poly").map(s => new File(tmpDir, s.format(prefix)))

      val coordIndices = withBufferedReader(outFiles.find(_.getName.endsWith(".ele")).get) { br =>
        // skip first line
        br.readLine()
        val pat = "\\s+".r
        Stream.continually(br.readLine).takeWhile(_ != null).flatMap { line =>
          if (line.startsWith("#"))
            None
          else {
            val triIndices = pat.split(line).slice(1, 4).map(_.toInt)
            triIndices
          }

        }.toArray
      }
      outFiles.foreach(_.delete())

      val indexedTriArray = new IndexedTriangleArray(pts.size, GeometryArray.COORDINATES | GeometryArray.COLOR_3, coordIndices.size)

      val idxMin = pts.indices.sortWith((i, j) => sf.vc.compare(i, j) < 0).head
      val pMin = pts(idxMin)
      val coords = pts.iterator.zipWithIndex.flatMap {
        case (p, i) =>
          Array(p(0) - pMin(0), p(1) - pMin(1), (sf.getFuncVal(i) - sf.getFuncVal(idxMin)) / 5.0)
      }.toArray

      val colors = pts.indices.map { i =>
        val f = sf.getFuncVal(i)
        new Color3f(colormap.Colormaps.getColor(f, sf.minFuncVal, sf.rangeFuncVal, colormap.Colormaps.CmapType.JET))
      }.toArray

      //      val colors = pts.indices.flatMap { i =>
      //        val f = sf.getFuncVal(i)
      //        
      ////        new Color3f(colormap.Colormaps.getColor(f, sf.minFuncVal, sf.rangeFuncVal, colormap.Colormaps.CmapType.JET))
      //      }.toArray

      indexedTriArray.setCoordinates(0, coords)
      indexedTriArray.setCoordinateIndices(0, coordIndices)
      indexedTriArray.setColors(0, colors)
      indexedTriArray.setColorIndices(0, coordIndices)

      val newWorldRoot = new BranchGroup
      val extrema = (for (i <- 0 until 3) yield {
        val c = pts.map(_(i))
        (c.min, c.max)
      }).toArray

      val minFuncVal = pts.indices.map(sf.getFuncVal).min
      val maxFuncVal = pts.indices.map(sf.getFuncVal).max
      val scale = 1.7f / (extrema.map(_._2).max - extrema.map(_._1).min)
      setViewScale(scale)

      val terrainAppear = new Appearance();
      terrainAppear.setColoringAttributes(new ColoringAttributes(0, 0, 0, ColoringAttributes.SHADE_GOURAUD));
      val pa = new PolygonAttributes()
      pa.setCullFace(PolygonAttributes.CULL_NONE)
      pa.setBackFaceNormalFlip(true)
      terrainAppear.setPolygonAttributes(pa)
      
            val material = new Material()
      material.setLightingEnable(false)
      material.setShininess(128f)
      terrainAppear.setMaterial(material)
      
      newWorldRoot.addChild(new Shape3D(indexedTriArray, terrainAppear))
      updateWorld(newWorldRoot)
//      simpleUniverse.getViewer.getView.setSceneAntialiasingEnable(true)

      publish(new TriangulatedPointCloudViewUpdated(indexedTriArray))
    }
  }
  */

  /*
  (new scala.swing.Frame {
    contents = Component.wrap(new javax.swing.JPanel {
      setLayout(new BorderLayout())
      add(triangulatedPointCloudView, BorderLayout.CENTER)
    })
    preferredSize = new Dimension(800, 800)
    visible = true
  })
  */

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
//      g2d.setColor(ColorSchemes.scheme.bgColor)
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
//      //      val 2dPts = 
//
//      //			List(p1, p2, p3, p4, p5).foreach(p => {
//      //			  val ellipse = new Ellipse2D.Double(p.x-r, p.y - r, 2*r, 2*r)
//      //			  g2d.draw(ellipse)
//      //			})
//
//      g2d.flush(true)
//
//      //(-0.08756825, 0.30226624, 0.0)
//      // (-0.11385262, 0.29914856, 0.028496)
//
//      //			getGraphics2D().flush(true)
//      //			val img = GraphicsUtilities.createTranslucentCompatibleImage(500, 500)
//      //			val g2d = img.createGraphics()
//      //			
//      //			g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
//      //			g2d.setColor(Color.white)
//      //			val stroke = new java.awt.BasicStroke
//      //			for (x <- 0 until 300 by 25) {
//      //				g2d.drawString("HELLO2", 10, x)
//      //				g2d.setStroke(new java.awt.BasicStroke(1))
//      //				val rect = new RoundRectangle2D.Double(x+4, img.getHeight() - 204, 20-4, 200, 5, 5)
//      ////				g2d.drawRect(x+4, img.getHeight() - 204, 20-4, 200)
//      //				g2d.draw(rect)
//      //			}
//      //			
//      //			getGraphics2D().drawAndFlushImage(img, 0, 0, null)
//    }
//  }

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

  val contourTreePanel = new ContourTreePanel(new Canvas3D(GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getBestConfiguration {
    new GraphicsConfigTemplate3D {
      setSceneAntialiasing(GraphicsConfigTemplate.PREFERRED)
    }
  })) {
    preferredSize = new Dimension(300, 500)
    //		reactions += {
    //			case e: DatasetLoaded => positions = null
    //			case ContourTreeReady(ct) => {
    //				publish(new ContourTreeUpdateEvent(ct))
    //			}
    //		}
  }

  val tcManager: TopologicalComponentManager = new TopologicalComponentManager(contourTreePanel, pointCloudView, terrainPanel)
  terrainPanel.tcManager = tcManager
  
  /*
	val dataPointViewer = new BorderPanel {
		class HistBar(val rect: RoundRectangle2D.Double) {
			val conformations = new scala.collection.mutable.ArrayBuffer[Conformation]
		}
		
		val selectedConformations = new scala.collection.mutable.ArrayBuffer[Conformation]
		val histBars = new Array[HistBar](16)
		
		val selectedRange: Component = new Component {
			preferredSize = new Dimension(90, 200)
			
			listenTo(mouse.clicks)
			
			
			reactions += {
				case MouseClicked(src, point, mod, clicks, pops) => {
					histBars.foreach(bar => {
						if (bar.rect.contains(point)) {
							if (bar.conformations.size > 0) {
								val c = bar.conformations(0)
								jmolPanel.viewer.openFile(c.pdbFile.getAbsolutePath)
								jmolPanel.viewer.script("cartoons only; set antialiasDisplay on; color structure;")
							}
						}
					})
				}
			}
			
			override def paintComponent(g2d: Graphics2D) = {
				
				def drawString(s: String, cx: Double, cy: Double): Unit = {
					val fontMetrics = g2d.getFontMetrics
					val layout = new java.awt.font.TextLayout(s, g2d.getFont, g2d.getFontRenderContext)
					
					val x = cx - layout.getBounds().getWidth / 2.0
					val y = cy + layout.getBounds().getHeight / 2.0
					layout.draw(g2d, x.toFloat, y.toFloat)
				}
				
				g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
				g2d.setColor(Color.black)
				g2d.fillRect(0, 0, bounds.width, bounds.height)
				
				
				val pad = 3
				val w = 80
				val h = 20
				(0 until histBars.size).foreach(binID => {
					val y = bounds.height - (binID) * (2*pad + h)
					val rect = new RoundRectangle2D.Double(pad, pad + y, w - 2*pad, h - 2*pad, 3, 3)
					histBars(binID) = new HistBar(rect)
				})
				
				if (selectedConformations.size != 0) {
					//val hist = Array.fill(16)(new scala.collection.mutable.ArrayBuffer[Conformation])
					val funcVals = selectedConformations.map(_.funcVal)
					val minFuncVal = funcVals.min
					val maxFuncVal = funcVals.max
					val rangeFuncVal = if (minFuncVal != maxFuncVal) maxFuncVal - minFuncVal else 1f
					
					selectedConformations.foreach(c => {
						val binID = math.min(((c.funcVal - minFuncVal) / rangeFuncVal * 16), 15).toInt
						histBars(binID).conformations += c
					})
					
					val minFreq = histBars.map(_.conformations.size).min
					val maxFreq = histBars.map(_.conformations.size).max
					
					(0 until histBars.size).foreach(binID => {
						val y = bounds.height - (binID) * (2*pad + h)
						val color = colormap.Colormaps.getColor(histBars(binID).conformations.size, minFreq, maxFreq - minFreq, colormap.Colormaps.CmapType.GRAY)
						g2d.setColor(color)
//						g2d.setColor(new Color(0, 255, 0, 128))
						val rect = new RoundRectangle2D.Double(pad, pad + y, w - 2*pad, h - 2*pad, 3, 3)
						g2d.fill(rect)
						g2d.draw(rect)
						
						g2d.setColor(Color.blue)
						drawString(histBars(binID).conformations.size.toString, pad + (w-pad)/2, y + pad + (h-pad)/2)
						
//						val y = (2*pad + h) * (16 - binID)
//						g2d.setColor(new Color(0, 255, 0, 128))
//						val rect = new RoundRectangle2D.Double(pad, pad + y, w, h + 2*pad, 3, 3)
//						g2d.fill(rect)
//						
//						g2d.setColor(Color.white)
//						drawString(hist(binID).size.toString, pad + w/2, y + pad + h/2)
						
					})
					
				}
			}
		}
		add(selectedRange, BorderPanel.Position.West)
		
		listenTo(tcManager)
		background = Color.blue
		preferredSize = new Dimension(450, 500)
		
		val jmolPanel = new JmolPanel()
		add(Component.wrap(jmolPanel), BorderPanel.Position.Center)
		
		/*
		jmolPanel.viewer.openFile(gammaModel.allConformations.reduceLeft((c1, c2) => {
			if (c1.funcVal < c2.funcVal) c1 else c2
		}).pdbFile.getAbsolutePath)
		jmolPanel.viewer.script("cartoons only; set antialiasDisplay on; color structure;")
		*/
		
		reactions += {
//			case NodeSelected(ctNode) => {
//				// Get file name of conformation
//				val selectedConformation = gammaModel.allConformations(ctNode.vertex)
//				jmolPanel.viewer.openFile(selectedConformation.pdbFile.getAbsolutePath)
//			}
			
			case TopologicalComponentManagerSelectionUpdate(tcManager) => {
				// Extract the selected conformations from the topological components
				val selectedSet = new scala.collection.mutable.HashSet[Conformation]
				
				for ((ctEdge, tcEntry) <- tcManager.componentMap if tcEntry.selected) {
					selectedSet += gammaModel.allConformations(ctEdge.n1.vertex)
					selectedSet += gammaModel.allConformations(ctEdge.n2.vertex)
					selectedSet ++= ctEdge.noncriticalNodes.map(n => gammaModel.allConformations(n.vertex))
				}
				
				selectedConformations.clear()
				selectedConformations ++= selectedSet
				
				selectedRange.repaint()
			}
			
		}
	}
	*/

  val conformationalSpacePanel = new ConformationalSpacePanel
  conformationalSpacePanel.pointsPanel.listenTo(tcManager)
  conformationalSpacePanel.pointsPanel.listenTo(terrainPanel)
  conformationalSpacePanel.ssPanel.listenTo(tcManager)

  val multiSplitPane = new MultiSplitPane
//  val div_1_2 = new Divider
//  val div_2_3 = new Divider
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
    //		modelRoot.setChildren(Arrays.asList(leftColumn, divLeftMiddle, middleColumn, divRightMiddle, rightColumn))
//    modelRoot.setChildren(Arrays.asList(col1, div_1_2, col2, div_2_3, col3, div_3_4, col4))
    modelRoot.setChildren(Arrays.asList(col3, div_3_4, col4))
    modelRoot.setRowLayout(true)

    multiSplitPane.setBackground(ColorSchemes.scheme.bgColor)
    multiSplitPane.getMultiSplitLayout().setModel(modelRoot)
    multiSplitPane.setDividerPainter(new DividerPainter {
      def paint(g: Graphics, div: Divider): Unit = {
        val g2d = g.asInstanceOf[Graphics2D]
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

        val bounds = div.getBounds

        //				val stroke = new BasicStroke(bounds.width / 2, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)

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

    //		val controlsPanel = n

    //		multiSplitPane.add(new Button("Left Column") {
    //			reactions += {case e: ButtonClicked => {this.preferredSize = new Dimension(0, 0); this.revalidate}}
    //		}.peer, "left");
//    multiSplitPane.add(pointCloudView, "col1")

//    multiSplitPane.add(contourTreePanel.peer, "col2");

    multiSplitPane.add(conformationalSpacePanel.peer, "col3")
    
//    new Frame {
//      title = "3D PCA Projection"
//      contents = pointCloudView
//      visible = true
//    }
    

    //		val terrainLayerPane = new LayeredPane {
    //		  peer.setLayout(new OverlayLayout(this.peer))
    //		  add(terrainPanel, new LayerConstraints(LayeredPane.Layer.Default))
    //		  
    //		  val panel = new Component with Container {
    //		    opaque = false
    //		    override def paintComponent(g2d: Graphics2D): Unit = {
    //		    	super.paintComponent(g2d)
    //		    }
    //		    
    //		    val btn = new Button("Hello!") {
    //		    	size = new Dimension(200, 200)
    //		    	peer.setLocation(new Point(500, 500))
    //		    }
    //		  	override def contents = List(btn)
    //		  }
    //		  
    //		  add(panel, new LayerConstraints(LayeredPane.Layer.Palette))
    //		}
    //		
    //		multiSplitPane.add(terrainLayerPane.peer, "col4");

    desktopPane = new DesktopPane(terrainPanel)
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

      //			add(terrainPanel, BorderPanel.Position.Center)
      add(Component.wrap(multiSplitPane), BorderPanel.Position.Center)
      //			add(statusBar, BorderPanel.Position.South)

      //			reactions += {
      //				case e: FocusLost => requestFocusInWindow()
      //			}
    }
    bp.focusable = true
    listenTo(bp.keys)
    bp
  }

}