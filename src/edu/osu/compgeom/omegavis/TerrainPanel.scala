package edu.osu.compgeom.omegavis

import java.awt.Graphics
import java.awt.BasicStroke
import java.awt.RenderingHints
import java.awt.AWTEvent
import com.sun.j3d.utils.scenegraph.transparency.TransparencySortController
import edu.osu.compgeom.omegavis.event.TerrainUpdatedEvent
import edu.osu.compgeom.ct._
import edu.osu.compgeom.dataset._
import edu.osu.compgeom.util.Timing
import edu.osu.compgeom.landscapes.floorplan._
import edu.osu.compgeom.landscapes._
import edu.osu.compgeom.topology.ScalarFunction
import edu.osu.compgeom.omegavis._
import edu.osu.compgeom.omegavis.event._
import javax.media.j3d._
import javax.vecmath._
import com.sun.j3d.utils.universe._
import com.sun.j3d.utils.image._
import com.sun.j3d.utils.geometry.Sphere
import com.sun.j3d.utils.behaviors.vp.OrbitBehavior
import com.sun.j3d.utils.geometry.GeometryInfo
import com.sun.j3d.utils.geometry.NormalGenerator
import java.awt.{ GraphicsConfigTemplate, GraphicsEnvironment, Point, Color }
import java.io.File
import java.awt.image.BufferedImage
import javax.media.jai.JAI
import scala.actors.Actor
import scala.actors.Actor._
import scala.swing._
import swing.event._
import javax.swing.JPanel
import java.awt.Color
import java.awt.BorderLayout
import java.awt.geom._
import edu.osu.compgeom.landscapes.floorplan.FloorplanElem
import edu.osu.compgeom.omegavis.behavior.MutatorBehavior
import scala.collection.immutable.NumericRange
import edu.osu.compgeom.omegavis.event.PathReady
import edu.osu.compgeom.j3d.MySJCanvas3D

class TerrainPanel(additionalPostRenderMethod: Canvas3D => Unit) extends MySJCanvas3D(new GraphicsConfigTemplate3D {
  setSceneAntialiasing(GraphicsConfigTemplate.REQUIRED)
}, additionalPostRenderMethod) {
  listenTo(this)

  var ct: ContourTree = null
  var tcManager: TopologicalComponentManager = null
  
  enableWakeupOnAWTEvents(AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_MOTION_EVENT_MASK | AWTEvent.MOUSE_WHEEL_EVENT_MASK)

  val canvas3D = this.offscreenCanvas3D

  var mouseX: Int = 0
  var mouseY: Int = 0

  val mutator = new MutatorBehavior
  mutator.setSchedulingBounds(new BoundingSphere(new Point3d(), 1000.0))

  sealed abstract class PickResult
  case class PickEdge(ctEdge: ContourTreeEdge) extends PickResult
  case class PickNode(ctNode: ContourTreeNode) extends PickResult
  case class PickConformation(conformationID: Int) extends PickResult

  val orbit: OrbitBehavior =
    //		var picked
    new OrbitBehavior(canvas3D, OrbitBehavior.REVERSE_ALL) {

      val oldColors = new scala.collection.mutable.HashMap[Shape3D, Array[Color4f]]
      override def processMouseEvent(evt: java.awt.event.MouseEvent): Unit = {
        if (evt.getID() == java.awt.event.MouseEvent.MOUSE_PRESSED &&
          (evt.getModifiers() & java.awt.event.InputEvent.SHIFT_MASK) == java.awt.event.InputEvent.SHIFT_MASK) {

          if ((evt.getModifiers() & java.awt.event.InputEvent.CTRL_MASK) != java.awt.event.InputEvent.CTRL_MASK)
            publish(new SelectionCleared()) // this might be a bug

          pickShapeAt(evt.getX, evt.getY) match {
            case Some(PickEdge(ctEdge)) => publish(new EdgeSelected(ctEdge, evt.getButton()))
            case Some(PickNode(ctNode)) => publish(new NodeSelected(ctNode))
            case Some(PickConformation(conformationID)) => {
              val allEdges = ct.criticalNodeToIncidentEdges.values.flatten.toArray
              val ctEdge = allEdges.find(e =>
                (e.n1.vertex == conformationID) || (e.n2.vertex == conformationID) || e.noncriticalNodes.iterator.map(_.vertex).contains(conformationID)).get
              println("Blah")

              // tcManager should listen to me.
              TerrainPanel.this.publish(new SelectionCleared())
              TerrainPanel.this.publish(new EdgeSelected(ctEdge, 1))

              // conformationalspacepanel.pointspanel should listen to me.
              TerrainPanel.this.publish(new TopologicalComponentManagerSelectionUpdate(tcManager))
              TerrainPanel.this.publish(new SetS1(conformationID))
            }
            case None => { /*Do nothing*/ }
          }
        } else if (evt.getID() == java.awt.event.MouseEvent.MOUSE_DRAGGED &&
          (evt.getModifiers() & java.awt.event.InputEvent.SHIFT_MASK) == java.awt.event.InputEvent.SHIFT_MASK) {
          pickShapeAt(evt.getX, evt.getY) match {
            case Some(PickEdge(ctEdge)) => {
              val selectionID = if ((evt.getModifiers() & java.awt.event.InputEvent.BUTTON1_MASK) == java.awt.event.InputEvent.BUTTON1_MASK) {
                1
              } else if ((evt.getModifiers() & java.awt.event.InputEvent.BUTTON2_MASK) == java.awt.event.InputEvent.BUTTON2_MASK) {
                2
              } else if ((evt.getModifiers() & java.awt.event.InputEvent.BUTTON3_MASK) == java.awt.event.InputEvent.BUTTON3_MASK) {
                3
              } else {
                0
              }
              publish(new EdgeSelected(ctEdge, selectionID))
            }
            case Some(PickNode(ctNode)) => publish(new NodeSelected(ctNode))
            case Some(PickConformation(conformationID)) => { /* Do nothing */ }
            case None => { /*Do nothing*/ }
          }
        } else if (evt.getID() == java.awt.event.MouseEvent.MOUSE_RELEASED &&
          (evt.getModifiers() & java.awt.event.InputEvent.BUTTON1_MASK) == java.awt.event.InputEvent.BUTTON1_MASK &&
          (evt.getModifiers() & java.awt.event.InputEvent.SHIFT_MASK) == java.awt.event.InputEvent.SHIFT_MASK) {
          println("Notifying listeners!")
          //TerrainPanel.this.publish(new SelectionUpdated(selectedEdges.toSet))
        } else if (evt.getID() == java.awt.event.MouseEvent.MOUSE_CLICKED) {
          publish(TerrainPanelClicked(evt.getPoint()))
          super.processMouseEvent(evt)
        } else if (evt.getID() == java.awt.event.MouseEvent.MOUSE_MOVED) {
          mouseX = evt.getX()
          mouseY = evt.getY()
        } else {
          super.processMouseEvent(evt)
        }
      }

      def pickShapeAt(x: Int, y: Int): Option[PickResult] = {
        val pickRay = createPickRay(x, y)
        val pickInfo = objRoot.pickClosest(PickInfo.PICK_GEOMETRY, PickInfo.NODE, pickRay)
        if (pickInfo == null) {
          return None
        }

        val s3d = pickInfo.getNode.asInstanceOf[Shape3D]
        if (s3d == null)
          return None

        s3d.getUserData match {
          case ctEdge: ContourTreeEdge => { return Some(new PickEdge(ctEdge)) }
          case ctNode: ContourTreeNode => { return Some(new PickNode(ctNode)) }
          case conformationID: java.lang.Integer => { return Some(new PickConformation(conformationID)) }
        }
      }

      def setShapeColors(s3d: Shape3D, highlight: Boolean): Unit = {
        val geomArray = s3d.getGeometry.asInstanceOf[TriangleStripArray]
        val terrainCoords = Array.fill[Point3f](geomArray.getVertexCount)(new Point3f)
        geomArray.getCoordinates(0, terrainCoords)

        val ctEdge = s3d.getUserData.asInstanceOf[ContourTreeEdge]
        if (highlight) {
          val originalColors = Array.fill[Color4f](geomArray.getVertexCount)(new Color4f)
          geomArray.getColors(0, originalColors)
          oldColors(s3d) = originalColors
          val colors = Array(new Color4f(Color.yellow))
          geomArray.setColors(0, colors)
          //					geomArray.setColorIndices(0, new Array[Int](terrainCoords.size))
        } else {
          val colors = oldColors(s3d)
          geomArray.setColors(0, colors)
          //					geomArray.setColorIndices(0, colors.indices.toArray)
        }
      }

      def createPickRay(x: Int, y: Int): PickRay = {
        val eye_pos = new Point3d
        val mouse_pos = new Point3d

        canvas3D.getCenterEyeInImagePlate(eye_pos)
        canvas3D.getPixelLocationInImagePlate(x, y, mouse_pos)

        val motion = new Transform3D
        canvas3D.getImagePlateToVworld(motion)
        motion.transform(eye_pos)
        motion.transform(mouse_pos)

        val direction = new Vector3d(mouse_pos)
        direction.sub(eye_pos)

        val pickRay = new PickRay(eye_pos, direction)
        return pickRay
      }
    }

  var simpleUniverse = new SimpleUniverse(canvas3D) {
    var view = getViewer().getView()
    view.setMinimumFrameCycleTime(17)
    view.setSceneAntialiasingEnable(true)
    view.setTransparencySortingPolicy(View.TRANSPARENCY_SORT_GEOMETRY)
    //		TransparencySortController.setComparator(view, null)
    ////		val c = new OrderingComparator
    //		view.setDepthBufferFreezeTransparent(true)
    var viewingPlatform = getViewingPlatform()
    viewingPlatform.setNominalViewingTransform()

    //		view.setProjectionPolicy(View.PARALLEL_PROJECTION)
    view.setFrontClipDistance(.01)
    val mouseBounds = new BoundingSphere(new Point3d(), 1000.0)
    orbit.setSchedulingBounds(mouseBounds)
    orbit.setProportionalZoom(true)
    orbit.setZoomFactor(.18)
    viewingPlatform.setViewPlatformBehavior(orbit)
  }

  var objRoot: BranchGroup = null
  var floorplan: VoronoiFloorplan = null
  var sf: ScalarFunction = null
  var linesBG: BranchGroup = null
  var spheresBG: BranchGroup = null

  var edgeColorMap = GammaColormaps.jet(_)

  reactions += {
    case PathReady(newPath) => {
      linesBG.detach()
      linesBG = new BranchGroup()
      linesBG.setCapability(BranchGroup.ALLOW_DETACH)

      val linesShape3D = new Shape3D(newPath)
      val lineAppear = new Appearance
      val lineAtts = new LineAttributes
      lineAtts.setLineAntialiasingEnable(true)
      lineAtts.setLineWidth(4)
      lineAppear.setLineAttributes(lineAtts)

      val colorAtts = new ColoringAttributes
      //      val lineColor = new Color3f(new Color(40, 40, 255))
      val lineColor = new Color3f(Color.yellow)
      colorAtts.setColor(lineColor.x, lineColor.y, lineColor.z)
      lineAppear.setColoringAttributes(colorAtts)
      linesShape3D.setAppearance(lineAppear)

      linesBG.addChild(linesShape3D)
      objRoot.addChild(linesBG)
    }

    case TerrainSpheresReady(newSpheresBG) => {
      spheresBG.detach()
      spheresBG = newSpheresBG
      objRoot.addChild(spheresBG)
    }

    case RegenerateTerrainEvent(ct, pointAtInfinity) => {
      sf = ct.scalarFunction
      val bspEnsemble = Timing("Computing BSP Tree Ensemble") {
        // Choose some local minimum to be the point at infinity

        BSPTreeEnsemble(ct, pointAtInfinity)
      }

      /*
			val w = 2000
			val h = 2000
			val img = new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR)
			val g2d = img.createGraphics()
			g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
			
			val ctLayout = ContourTree2DLayout(bspEnsemble, ct)
			val stack = new scala.collection.mutable.Stack[FloorplanElem[NumericRange[Double]]]
			stack.pushAll(ctLayout.rootElems)
			
			ctLayout.rootElems.foreach(root => {
				val fOuter = ct.scalarFunction.getFuncVal(root.bspNode.outerNode.vertex)
				
				val x1 = root.contour.start / 100.0 * w
				val x2 = root.contour.end / 100.0 * w
				
				val x = (x1 + x2) / 2.0
				
				val y1 = h - (fOuter - ct.scalarFunction.minFuncVal) / ct.scalarFunction.rangeFuncVal * h
				
				g2d.setColor(new Color(128, 0, 0, 128))
				g2d.fillOval(x.toInt-3, y1.toInt-3, 7, 7)
				
				
			})
			
			while (!stack.isEmpty) {
				val floorplanElem = stack.pop
				val fOuter = ct.scalarFunction.getFuncVal(floorplanElem.bspNode.outerNode.vertex)
				val fInner = ct.scalarFunction.getFuncVal(floorplanElem.bspNode.innerNode.vertex)
				
				val x1 = floorplanElem.contour.start / 100.0 * w
				val x2 = floorplanElem.contour.end / 100.0 * w
				
				val x = (x1 + x2) / 2.0
				
				val y1 = h - (fOuter - ct.scalarFunction.minFuncVal) / ct.scalarFunction.rangeFuncVal * h
				val y2 = h - (fInner - ct.scalarFunction.minFuncVal) / ct.scalarFunction.rangeFuncVal * h
				
				// Draw line from outer critical point to inner critical point
				g2d.setColor(new Color(0, 255, 0, 128))
//				g2d.draw(curve)
				g2d.drawLine(x.toInt, y1.toInt, x.toInt, y2.toInt)
				
				// Draw noncritical points
				floorplanElem.bspNode.ctEdge.noncriticalNodes.foreach(ncNode => {
					val f = ct.scalarFunction.getFuncVal(ncNode.vertex)
					val y = h - (f - ct.scalarFunction.minFuncVal) / ct.scalarFunction.rangeFuncVal * h
					val c = new Color(Color.orange.getRed, Color.orange.getGreen, Color.orange.getBlue, 128)
					g2d.setColor(c)
					g2d.fillOval(x.toInt-3, y.toInt-3, 7, 7)
				})
				
				// Draw lines from inner critical point to each child's outer critical point
				floorplanElem.children.foreach(child => {
					val fc = ct.scalarFunction.getFuncVal(child.bspNode.outerNode.vertex)
					val x1c = child.contour.start / 100.0 * w
					val x2c = child.contour.end / 100.0 * w
					val yc = h - (fc - ct.scalarFunction.minFuncVal) / ct.scalarFunction.rangeFuncVal * h
					val xc = (x1c + x2c) / 2.0
					
//					val curve = new CubicCurve2D.Double(x, y, x, (y + yc) / 2.0, xc, (y + yc) / 2.0, xc, yc)
					
					g2d.setColor(new Color(0, 255, 0, 128))
//					g2d.draw(curve)
					g2d.drawLine(x.toInt, y2.toInt, xc.toInt, yc.toInt)
					
				})
				
				g2d.setColor(new Color(0, 0, 128, 128))
				g2d.fillOval(x.toInt-3, y2.toInt-3, 7, 7)
				
				stack.pushAll(floorplanElem.children)
			}
			JAI.create("filestore", img, "/dev/shm/ct.png", "PNG")
			*/

      //			val floorplan = SliceAndDiceFloorplan(bspEnsemble, ct, 11, edgeColorMap)
      floorplan = VoronoiFloorplan(bspEnsemble, ct, pctChange = .01d /*, epsilon=1e-2, delta=1e-99, pctChange=1d*/ )
      updateScene(ct, floorplan)

      publish(new TerrainUpdatedEvent(floorplan, ct))

    }
    case SetCameraEvent(rotationCenter, height) => {
      orbit.setRotationCenter(rotationCenter)
      //			orbit.setZoomFactor(.01)

      val tg = simpleUniverse.getViewingPlatform().getMultiTransformGroup().getTransformGroup(0);

      // New code
      val viewTrans = new Transform3D
      viewTrans.setIdentity
      viewTrans.lookAt(new Point3d(rotationCenter.x, rotationCenter.y, 1), rotationCenter, new Vector3d(0, 1, 0))
      viewTrans.invert
      simpleUniverse.getViewingPlatform().getViewPlatformTransform().setTransform(viewTrans)
      canvas3D.repaint
    }
  }

  def getCameraTransform: Array[Double] = {
    val tg = simpleUniverse.getViewingPlatform().getMultiTransformGroup().getTransformGroup(0)
    val viewTrans = new Transform3D
    tg.getTransform(viewTrans)
    val ret = new Array[Double](16)
    viewTrans.get(ret)
    ret
  }

  def updateScene(ct: ContourTree,
    floorplan: VoronoiFloorplan) = {
    
    this.ct = ct
    // TODO:  Get the locale, remove the branch graph, and re-add.
    if (objRoot != null) {
      objRoot.detach
    }

    objRoot = new BranchGroup()
    objRoot.setCapability(BranchGroup.ALLOW_DETACH)
    objRoot.setCapability(Group.ALLOW_CHILDREN_WRITE)
    objRoot.setCapability(Group.ALLOW_CHILDREN_EXTEND)

    objRoot.addChild(mutator)

    //		val testBehavior = new Behavior {
    //			def initialize() {
    //				println("Yay")
    //				wakeupOn(new WakeupOnAWTEvent(java.awt.event.ActionEvent.ACTION_PERFORMED))
    //			}
    //			
    //			def processStimulus(criteria: java.util.Enumeration[_]): Unit = {
    //				println("Yay 2")
    //				wakeupOn(new WakeupOnAWTEvent(java.awt.event.ActionEvent.ACTION_PERFORMED))
    //			}
    //		}
    //		testBehavior.setSchedulingBounds(new BoundingSphere(new Point3d(), 1000.0))
    //		objRoot.addChild(testBehavior)

    //		val bgFileName = "gridlight.png"
    val bounds = new BoundingSphere(new Point3d(), 100.0)
    //		val bgTexture = new TextureLoader(bgFileName, canvas3D)
    //val bg = new Background(bgTexture.getImage)
    val bg = new Background(new Color3f(ColorSchemes.scheme.bgColor))
    bg.setImageScaleMode(Background.SCALE_NONE_CENTER)
    bg.setApplicationBounds(bounds)
    val bgBranchGroup = new BranchGroup()
    bgBranchGroup.setCapability(BranchGroup.ALLOW_DETACH)
    bgBranchGroup.addChild(bg)
    objRoot.addChild(bgBranchGroup)

    //				BranchGroup objRoot = new BranchGroup();
    //		objRoot.setCapability(BranchGroup.ALLOW_DETACH);
    //		objRoot.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
    //		objRoot.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);
    //		
    //		//String bgFileName = "Above_the_Clouds_2.jpg";
    //		String bgFileName = "gridlight.png";
    //		
    //		// Create a bounds for the background and lights
    //		BoundingSphere bounds =
    //		    new BoundingSphere(new Point3d(0.0,0.0,0.0), 100.0);
    //		TextureLoader bgTexture = new TextureLoader(bgFileName, this.canvas3D);
    //		Background bg = new Background(bgTexture.getImage());
    //		bg.setImageScaleMode(Background.SCALE_FIT_ALL);
    //		bg.setApplicationBounds(bounds);
    //		objRoot.addChild(bg);
    //		
    val lightDirs = List(
      new Vector3f(0, 0, -1),
      new Vector3f(1, 0, -1),
      new Vector3f(-1, -1, -1),
      new Vector3f(1, 1, -1),
      new Vector3f(0, 0, 1),
      new Vector3f(1, 0, 0),
      new Vector3f(1, 1, 1),
      new Vector3f(-1, 1, 1))

    val lColor = new Color3f(0.5f, 0.5f, 0.5f);
    for (dir <- lightDirs) {
      val light = new DirectionalLight(lColor, dir)
      light.setInfluencingBounds(bounds)
      objRoot.addChild(light)
    }

    val alColor = new Color3f(.5f, .5f, .5f)
    val ambientLight = new AmbientLight(alColor)
    ambientLight.setInfluencingBounds(bounds)
    objRoot.addChild(ambientLight)

    def makeAppearance() = {
      val terrainAppear = new Appearance()
      terrainAppear.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_READ)
      terrainAppear.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE)
      val transAtts = new TransparencyAttributes(TransparencyAttributes.BLENDED, 0f, TransparencyAttributes.BLEND_SRC_ALPHA, TransparencyAttributes.BLEND_ONE_MINUS_SRC_ALPHA)
      transAtts.setCapability(TransparencyAttributes.ALLOW_MODE_WRITE)
      transAtts.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE)
      terrainAppear.setTransparencyAttributes(transAtts)

      val coloringAttributes = new ColoringAttributes(0, 0, 0, ColoringAttributes.NICEST)
      terrainAppear.setColoringAttributes(coloringAttributes)

      val lineAttributes = new LineAttributes()
      lineAttributes.setLineAntialiasingEnable(true)
      terrainAppear.setLineAttributes(lineAttributes)

      val material = new Material()
      material.setLightingEnable(true)
      material.setShininess(128f)
      terrainAppear.setMaterial(material)

      val rendAtts = new RenderingAttributes
      terrainAppear.setCapability(Appearance.ALLOW_RENDERING_ATTRIBUTES_READ)
      terrainAppear.setCapability(Appearance.ALLOW_RENDERING_ATTRIBUTES_WRITE)
      rendAtts.setCapability(RenderingAttributes.ALLOW_VISIBLE_READ)
      rendAtts.setCapability(RenderingAttributes.ALLOW_VISIBLE_WRITE)
      terrainAppear.setRenderingAttributes(rendAtts)

      val pa = new PolygonAttributes()
      pa.setCullFace(PolygonAttributes.CULL_NONE)
      pa.setBackFaceNormalFlip(true)
      terrainAppear.setPolygonAttributes(pa)
      terrainAppear
    }

    floorplan.topoComponents.foreach(t => {

      t.setAppearance(makeAppearance())
      objRoot.addChild(t)
    })

    linesBG = new BranchGroup
    linesBG.setCapability(BranchGroup.ALLOW_DETACH)
    objRoot.addChild(linesBG)

    spheresBG = new BranchGroup
    spheresBG.setCapability(BranchGroup.ALLOW_DETACH)
    objRoot.addChild(spheresBG)

    //		objRoot.addChild(floorplan.sphereBranchGroup)

    //		val info = new GeometryInfo(geomArray)
    //		info.recomputeIndices
    //		val generator = new NormalGenerator()
    //		Timing("Generating normals") {
    //			generator.generateNormals(info)
    //		}
    //		geomArray.setNormals(0, info.getNormals)
    //		geomArray.asInstanceOf[IndexedTriangleArray].setNormalIndices(0, info.getNormalIndices)
    //		
    //		val shape3D = new Shape3D(geomArray, terrainAppear)
    //		objRoot.addChild(shape3D)

    println("Updated the view!")

    objRoot.compile

    simpleUniverse.addBranchGraph(objRoot)

  }
}

object TerrainPanel {
}

case class RegenerateTerrainEvent(val ct: ContourTree, val nodeAtInfinity: ContourTreeNode) extends Event