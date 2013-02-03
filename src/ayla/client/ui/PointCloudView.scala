/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui

import scala.swing.Reactor
import scala.collection.mutable.Subscriber
import scala.swing.Publisher
import java.awt.Dimension
import java.awt.BorderLayout
import javax.swing.JFrame
import com.sun.j3d.utils.image.TextureLoader
import javax.vecmath.{Point3f, Point3d, Color3f, Vector3f, Vector3d}
import com.sun.j3d.utils.behaviors.vp.OrbitBehavior
import com.sun.j3d.utils.universe.SimpleUniverse
import java.awt.{GraphicsConfiguration, Graphics2D, GradientPaint, Color}
import java.awt.image.BufferedImage
import java.io._
import javax.media.j3d._
import com.sun.j3d.utils.geometry._
import java.awt.event.MouseEvent
import ayla.dataset.CachedDataset
import ayla.pdb.FilePdbStreamProvider
import ayla.j3d.MySJCanvas3D
import java.awt.GraphicsConfigTemplate
import java.awt.AWTEvent

class PointCloudView extends MySJCanvas3D(new GraphicsConfigTemplate3D {
      setSceneAntialiasing(GraphicsConfigTemplate.REQUIRED)
    }, (c: Canvas3D) => {}) {
  
  listenTo(this)
  enableWakeupOnAWTEvents(AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_MOTION_EVENT_MASK | AWTEvent.MOUSE_WHEEL_EVENT_MASK)
  val canvas3d = this.offscreenCanvas3D
	
	val pickListeners = new scala.collection.mutable.ListBuffer[Shape3D => Unit]
	
	val objRoot = new BranchGroup
	objRoot.setCapability(Group.ALLOW_CHILDREN_WRITE)
	objRoot.setCapability(Group.ALLOW_CHILDREN_EXTEND)
	
	val masterTransformGroup = new TransformGroup
	masterTransformGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_READ)
	masterTransformGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
	masterTransformGroup.setTransform(new Transform3D)
	masterTransformGroup.setCapability(Group.ALLOW_CHILDREN_WRITE)
	masterTransformGroup.setCapability(Group.ALLOW_CHILDREN_EXTEND)
	objRoot.addChild(masterTransformGroup)
	
	private var worldRoot: BranchGroup = null
	
	// Set up the java 3d stuff
	val simpleUniverse: SimpleUniverse = new SimpleUniverse(canvas3d)
	initialize()
	
	def updateWorld(worldBranchGroup: BranchGroup) = {
		if (worldRoot != null) {
			worldRoot.detach()
		}
		worldBranchGroup.setCapability(BranchGroup.ALLOW_DETACH)
		worldBranchGroup.compile
		masterTransformGroup.addChild(worldBranchGroup)
		worldRoot = worldBranchGroup
		
	}
	
	def setViewScale(scale: Double): Unit = {
		val t3D = new Transform3D
		t3D.setScale(scale)
		masterTransformGroup.setTransform(t3D)
	}
	
	lazy val pickingOrbitBehavior: OrbitBehavior = 
		new OrbitBehavior(canvas3d, OrbitBehavior.REVERSE_ALL) {
			override def processMouseEvent(evt: MouseEvent): Unit = {
				if (evt.getButton() == MouseEvent.BUTTON3 && evt.getID() == MouseEvent.MOUSE_PRESSED) {
					val pickRay = createPickRay(evt.getX, evt.getY)
					val sgPath = objRoot.pickAny(pickRay)
					if (sgPath == null) 
						return
					
					val pickedNode = sgPath.getObject.asInstanceOf[Shape3D]
					pickListeners.foreach(_(pickedNode))
				}
				super.processMouseEvent(evt)
			}
			
			def createPickRay(x: Int, y: Int): PickRay = {
				val eye_pos = new Point3d
				val mouse_pos = new Point3d
				
				canvas3d.getCenterEyeInImagePlate(eye_pos)
				canvas3d.getPixelLocationInImagePlate(x, y, mouse_pos)
				
				val motion = new Transform3D
				canvas3d.getImagePlateToVworld(motion)
				motion.transform(eye_pos)
				motion.transform(mouse_pos)
				
				val direction = new Vector3d(mouse_pos)
				direction.sub(eye_pos)
				
				return new PickRay(eye_pos, direction)
			}
		}

	def initialize(): Unit = {
		simpleUniverse.getViewer().getView().setMinimumFrameCycleTime(17)
		val viewingPlatform = simpleUniverse.getViewingPlatform()
		viewingPlatform.setNominalViewingTransform()
		val mouseBounds = new BoundingSphere(new Point3d(), 1000.0)
		pickingOrbitBehavior.setSchedulingBounds(mouseBounds)
		pickingOrbitBehavior.setProportionalZoom(true)
		pickingOrbitBehavior.setZoomFactor(.18)
		viewingPlatform.setViewPlatformBehavior(pickingOrbitBehavior)
		simpleUniverse.getViewer().getView().setFrontClipDistance(.00001)
	
		/*
		// Set up background image
		val bgTile = new BufferedImage(15, 15, BufferedImage.TYPE_3BYTE_BGR)
		val g2d = bgTile.getGraphics.asInstanceOf[Graphics2D]
		g2d.setColor(java.awt.Color.white)
		g2d.fillRect(0, 0, 15, 15)
		g2d.setColor(new java.awt.Color(0, 255, 0, 50))
		g2d.drawLine(0, 0, 0, 15)
		g2d.drawLine(0, 0, 15, 0)
		val bounds = new BoundingSphere(new Point3d(0.0,0.0,0.0), 100.0)
		val bgTexture = new TextureLoader(bgTile, simpleUniverse.getCanvas())
		val bg = new Background(bgTexture.getImage())
		bg.setImageScaleMode(Background.SCALE_REPEAT)
		*/
		
		val bgImage = new BufferedImage(5, 5, BufferedImage.TYPE_3BYTE_BGR)
		val g2d = bgImage.getGraphics.asInstanceOf[Graphics2D]
		g2d.setColor(ColorSchemes.scheme.bgColor)
		g2d.fillRect(0, 0, 1000, 1000)
		val bounds = new BoundingSphere(new Point3d(0, 0, 0), 100)
		val bgTexture = new TextureLoader(bgImage, simpleUniverse.getCanvas())
		val bg = new Background(bgTexture.getImage())
		bg.setImageScaleMode(Background.SCALE_FIT_ALL)
		
		bg.setApplicationBounds(bounds)
		val bgBranchGroup = new BranchGroup()
		bgBranchGroup.setCapability(BranchGroup.ALLOW_DETACH)
		bgBranchGroup.addChild(bg)
		masterTransformGroup.addChild(bgBranchGroup)
		
		simpleUniverse.addBranchGraph(objRoot)
	}
}

//object PointCloudView {
//	def main(args: Array[String]): Unit = {
//		val dataset = new CachedDataset(new File("/home/harveywi/research/ScalaCrystals/Torus_1000")) with FilePdbStreamProvider
//		
//		val pcdView = new PointCloudView
//		
//		val mainFrame = new JFrame("Application")
//		mainFrame.getContentPane().setLayout(new BorderLayout())
//		mainFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
//
//		mainFrame.getContentPane().add(pcdView.peer, BorderLayout.CENTER);
//		mainFrame.setSize(new Dimension(800, 600));
//		mainFrame.setVisible(true);
//		
//		val worldRoot = new BranchGroup
//		
//		val boxSize = 0.05f
//		dataset.pcaPoints.foreach {p =>
//			val tVec = new Vector3f(p(0), p(1), p(2))
//			
//			val trans = new Transform3D()
//			trans.setTranslation(tVec)
//			trans.setScale(boxSize)
//			val transGroup = new TransformGroup(trans)
////			transGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
////			transGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_READ)
//			
//			worldRoot.addChild(transGroup)
//			
//			val sphereAppearance = new Appearance()
//			sphereAppearance.setCapability(Appearance.ALLOW_COLORING_ATTRIBUTES_WRITE)
//
//			val sphere = new Sphere(.2f, sphereAppearance)
//			sphere.setAppearance(sphereAppearance)
//			val color = java.awt.Color.green//Colormaps.getColor(energies[i], minVal, range, Colormaps.CmapType.JET);
//			val ca = new ColoringAttributes(new Color3f(color), ColoringAttributes.SHADE_FLAT)
//			sphere.getAppearance().setColoringAttributes(ca)
//			transGroup.addChild(sphere);
//			
//		}
//		
//		pcdView.updateWorld(worldRoot)
//		
//		val newWorldRoot = new BranchGroup
//		dataset.pcaPoints.foreach {p =>
//			val tVec = new Vector3f(p(0), p(1), p(2))
//			
//			val trans = new Transform3D()
//			trans.setTranslation(tVec)
//			trans.setScale(boxSize)
//			val transGroup = new TransformGroup(trans)
////			transGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
////			transGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_READ)
//			
//			newWorldRoot.addChild(transGroup)
//			
//			val sphereAppearance = new Appearance()
//			sphereAppearance.setCapability(Appearance.ALLOW_COLORING_ATTRIBUTES_WRITE)
//
//			val sphere = new Sphere(.2f, sphereAppearance)
//			sphere.setAppearance(sphereAppearance)
//			val color = java.awt.Color.red//Colormaps.getColor(energies[i], minVal, range, Colormaps.CmapType.JET);
//			val ca = new ColoringAttributes(new Color3f(color), ColoringAttributes.SHADE_FLAT)
//			sphere.getAppearance().setColoringAttributes(ca)
//			transGroup.addChild(sphere);
//			
//		}
//		
////		pcdView.setViewScale(.3)
//		
//		pcdView.updateWorld(newWorldRoot)
//		
//	}
//}
