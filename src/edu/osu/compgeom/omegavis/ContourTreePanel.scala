package edu.osu.compgeom.omegavis

import edu.osu.compgeom.omegavis.event.TerrainUpdatedEvent
import java.awt.geom.Point2D
import edu.osu.compgeom.landscapes.floorplan._
import edu.osu.compgeom.landscapes.floorplan.VoronoiFloorplan
import edu.osu.compgeom.omegavis.event._
import java.awt.event.ActionListener
import javax.swing.Timer
import org.jgrapht.traverse.DepthFirstIterator
import org.jgrapht.alg.DijkstraShortestPath
import org.jgrapht.graph._
import org.jgrapht.traverse.TopologicalOrderIterator
import org.jgrapht.alg.FloydWarshallShortestPaths
import org.jgrapht.graph.SimpleWeightedGraph
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.SimpleGraph
import edu.osu.compgeom.topology.ScalarFunction
import scala.collection.JavaConversions._
import edu.osu.compgeom.ct._
import edu.osu.compgeom.dataset._
import javax.media.j3d._
import javax.vecmath._
import com.sun.j3d.utils.universe._
import com.sun.j3d.utils.image._
import com.sun.j3d.utils.geometry.Sphere
import com.sun.j3d.utils.behaviors.vp.OrbitBehavior
import java.awt.{GraphicsConfigTemplate, GraphicsEnvironment}
import java.io.File
import java.awt.geom.Point2D.{Double => Pt}
import scala.actors.Actor
import scala.actors.Actor._
import swing._
import swing.event._
import scala.swing.test.SimpleApplet
import javax.swing.JPanel
import java.awt.Color
import java.awt.BorderLayout
import edu.osu.compgeom.omegavis.behavior.MutatorBehavior
import edu.osu.compgeom.util.UnionFind
import org.interactivemesh.scala.swing.j3d.SCanvas3D

class ContourTreePanel(val canvas3D: Canvas3D) extends SCanvas3D(canvas3D) {
	listenTo(this)
	
	var mutator: MutatorBehavior = null
	
//	var canvas3D = new Canvas3D(
//		GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getBestConfiguration {
//			new GraphicsConfigTemplate3D {
//				setSceneAntialiasing(GraphicsConfigTemplate.PREFERRED)
//			}
//		}
//	)
	
	val simpleUniverse = new SimpleUniverse(canvas3D)
		var view = simpleUniverse.getViewer().getView()
		view.setMinimumFrameCycleTime(17)
		view.setFrontClipDistance(.01)
		view.setSceneAntialiasingEnable(true)
		var viewingPlatform = simpleUniverse.getViewingPlatform()
		viewingPlatform.setNominalViewingTransform()
//		view.setProjectionPolicy(View.PARALLEL_PROJECTION)
		
//				ViewingPlatform viewingPlatform = simpleUniverse.getViewingPlatform();
//		viewingPlatform.setNominalViewingTransform();
//		PickingOrbitBehavior orbit = new PickingOrbitBehavior(canvas3D,
//				OrbitBehavior.REVERSE_ALL, this);
//		orbit.setRotYFactor(0);
//		BoundingSphere mouseBounds = new BoundingSphere(new Point3d(), 1000.0);
//		orbit.setSchedulingBounds(mouseBounds);
//		viewingPlatform.setViewPlatformBehavior(orbit);
		
	val orbit = new OrbitBehavior(canvas3D, OrbitBehavior.REVERSE_ALL)
		orbit.setRotYFactor(0)
		orbit.setProportionalZoom(true)
		orbit.setZoomFactor(.18)
		val mouseBounds = new BoundingSphere(new Point3d(), 1000.0)
		orbit.setSchedulingBounds(mouseBounds)
		viewingPlatform.setViewPlatformBehavior(orbit)
	
	var ready = true
	var objRoot: BranchGroup = null
	
	var ctEdgesBranchGroup: BranchGroup = null
	var cmap = GammaColormaps.jet(_)
	var ct: ContourTree = null
	
	var lines: Array[Shape3D] = null
	
	reactions += {
		case SetCameraEvent(rotationCenter, zoom) => {
			orbit.setRotationCenter(rotationCenter)
			
			val tg = simpleUniverse.getViewingPlatform().getMultiTransformGroup().getTransformGroup(0);

			// New code
			val viewTrans = new Transform3D
			viewTrans.setIdentity
			viewTrans.lookAt(new Point3d(rotationCenter.x, rotationCenter.y, 1), rotationCenter, new Vector3d(0, 1, 0))
			viewTrans.invert
			simpleUniverse.getViewingPlatform().getViewPlatformTransform().setTransform(viewTrans)
			canvas3D.repaint
		}
		
		case SelectionUpdated(selectedEdges: Set[ContourTreeEdge]) => {
			val sf = ct.scalarFunction
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
			
			lines.foreach(s3d => {
				val line = s3d.getGeometry().asInstanceOf[LineArray]
				val e = line.getUserData.asInstanceOf[ContourTreeEdge]
				val c1 = getNodeColor(sf.getFuncVal(e.n1.vertex), e)
				val c2 = getNodeColor(sf.getFuncVal(e.n2.vertex), e)
				line.setColors(0, Array(c1, c2))
			})
		}
		
		case TerrainUpdatedEvent(floorplan, ct) => {
			ready = false
			this.ct = ct
			updateScene(floorplan)
			ready = true
		}
		
//		case ContourTreeReady(ct) => {
//			ready = false
//			this.ct = ct
//			ready = true
//		}
	}
	
//	var positions: Map[Int, Point3f] = null
	
	def getPositions(floorplan: VoronoiFloorplan): Map[Int, Point3f] = {
		val positions = new scala.collection.mutable.HashMap[Int, Point3f]
		val posList = new scala.collection.mutable.ArrayBuffer[Point3f]
		val stack = new scala.collection.mutable.Stack[FloorplanElem[Array[Point2D]]]
		stack.pushAll(floorplan.floorplan.rootElems)
		
		floorplan.floorplan.rootElems.foreach(elem => {
			val node = elem.bspNode.outerNode
			val funcVal = ct.scalarFunction.getFuncVal(node.vertex)
			val center = PolygonUtilities.centerOfMass(elem.contour)
			val p = new Point3f(center.getY.toFloat, funcVal, center.getX.toFloat)
			positions(node.vertex) = p
			posList += p
		})
		
		while (!stack.isEmpty) {
			val elem = stack.pop
			
			val node = elem.bspNode.innerNode
			val funcVal = ct.scalarFunction.getFuncVal(node.vertex)
			val center = if (!elem.contour.isEmpty) PolygonUtilities.centerOfMass(elem.contour) else new Point2D.Double(0, 0)
			if (center.getX.isNaN || center.getY.isNaN || center.getX.isInfinite || center.getY.isInfinite) {
				center.setLocation(elem.contour(0).getX, elem.contour(0).getY)
			}
			val p = new Point3f(center.getY.toFloat, funcVal, center.getX.toFloat)
			positions(node.vertex) = p
			posList += p
			stack.pushAll(elem.children)
		}
		
//		assert(posList.size == ct.nodesContracted.size, "Positions:  " + posList.size + "\tCT Nodes:  " + ct.nodesContracted.size)
		
		// Normalize the node positions
		val minPos = posList.foldLeft(new Point3f(Float.MaxValue, Float.MaxValue, Float.MaxValue))(
				(curMin, p) => new Point3f(math.min(curMin.x, p.x), math.min(curMin.y, p.y), math.min(curMin.z, p.z))
		)
		val maxPos = posList.foldLeft(new Point3f(-Float.MaxValue, -Float.MaxValue, -Float.MaxValue))(
				(curMax, p) => new Point3f(math.max(curMax.x, p.x), math.max(curMax.y, p.y), math.max(curMax.z, p.z))
		)
		
		val range = new Point3f(maxPos.x - minPos.x, maxPos.y - minPos.y, maxPos.z - minPos.z)
		
		for (p <- posList) {
			p.x = (p.x - minPos.x)
			p.y = (p.y - minPos.y)
			p.z = (p.z - minPos.z)
			
			if (range.x != 0)
				p.x = p.x / range.x - .5f
				
			if (range.y != 0)
				p.y = p.y / range.y - .5f
			
			if (range.z != 0)
				p.z = p.z / range.z - .5f
		}
		
		return positions.toMap
	}
	
	def detachEdges(): Unit = {
		ctEdgesBranchGroup.detach
	}
	
	def attachEdges(): Unit = {
		objRoot.addChild(ctEdgesBranchGroup)
	}
	
	def updateScene(floorplan: VoronoiFloorplan) = {
		// TODO:  Get the locale, remove the branch graph, and re-add.
		if (objRoot != null) {
			objRoot.detach
		}
		
//		if (positions == null) {
//			positions = getPositions(floorplan)
//		}
		
		val positions = getPositions(floorplan)
		
		objRoot = new BranchGroup()
		objRoot.setCapability(BranchGroup.ALLOW_DETACH)
		objRoot.setCapability(Group.ALLOW_CHILDREN_WRITE)
		objRoot.setCapability(Group.ALLOW_CHILDREN_EXTEND)
		
		mutator = new MutatorBehavior
		mutator.setSchedulingBounds(new BoundingSphere(new Point3d(), 1000.0))
		
		objRoot.addChild(mutator)
		
//		val bgFileName = "grid.png"
		val bounds = new BoundingSphere(new Point3d(), 100.0)
//		val bgTexture = new TextureLoader(bgFileName, canvas3D)
//		val bg = new Background(bgTexture.getImage)
		val bg = new Background(new Color3f(ColorSchemes.scheme.bgColor))
		bg.setImageScaleMode(Background.SCALE_NONE_CENTER)
		bg.setApplicationBounds(bounds)
		val bgBranchGroup = new BranchGroup()
		bgBranchGroup.setCapability(BranchGroup.ALLOW_DETACH)
		bgBranchGroup.addChild(bg)
		objRoot.addChild(bgBranchGroup)
		
		val sphereBranchGroup = new BranchGroup()
		val coords = new scala.collection.mutable.ListBuffer[Float]
		for (n <- ct.nodesContracted) {
			val nPos = positions(n.vertex)
			
			for (parent <- n.parents) {
				val parentPos = positions(parent.vertex)
				coords ++= List(nPos.x, nPos.y, nPos.z, parentPos.x, parentPos.y, parentPos.z)
			}
			
			val t = new Transform3D()
			t.setTranslation(new Vector3f(nPos))
			val tg = new TransformGroup(t)
			
			val appearance = new Appearance()
			appearance.setCapability(Appearance.ALLOW_COLORING_ATTRIBUTES_WRITE)
			val sphere = new Sphere(.0005f)
			sphere.setAppearance(appearance)
			sphere.setPickable(true)
			sphere.getShape().setUserData(n)
			
			val colorAtts = new ColoringAttributes(new Color3f(Color.lightGray), ColoringAttributes.SHADE_GOURAUD);
			sphere.getAppearance().setColoringAttributes(colorAtts);
			
			val transAtts = new TransparencyAttributes(TransparencyAttributes.NICEST, .5f)
			sphere.getAppearance().setTransparencyAttributes(transAtts)
			
			tg.addChild(sphere);
			sphereBranchGroup.addChild(tg);
		}
		objRoot.addChild(sphereBranchGroup)
		
		val ctEdges = ct.criticalNodeToIncidentEdges.values.flatten.toSet
	  ctEdgesBranchGroup = new BranchGroup
	  ctEdgesBranchGroup.setCapability(BranchGroup.ALLOW_DETACH)
	  objRoot.addChild(ctEdgesBranchGroup)
		lines = (ctEdges.map(e => {
			val line = new LineArray(2, GeometryArray.COORDINATES | GeometryArray.COLOR_4)
			line.setCapability(GeometryArray.ALLOW_COLOR_READ)
			line.setCapability(GeometryArray.ALLOW_COLOR_WRITE)
			line.setCapability(GeometryArray.ALLOW_COORDINATE_READ)
			val p1 = positions(e.n1.vertex)
			val p2 = positions(e.n2.vertex)
			line.setCoordinates(0, Array(p1, p2))
			line.setColors(0, Array.fill(2)(new Color4f()))
			
			val appearance = new Appearance()
			val lineAtts = new LineAttributes
			lineAtts.setLineAntialiasingEnable(true)
			lineAtts.setLineWidth(3f)
			appearance.setLineAttributes(lineAtts)
			
			val s3d = new Shape3D(line)
			s3d.setCapability(Shape3D.ALLOW_GEOMETRY_READ)
			s3d.setUserData(e)
			s3d.setAppearance(appearance)
			
			ctEdgesBranchGroup.addChild(s3d)
			s3d
		})).toArray
		
		/*
		val ctEdges = ct.criticalNodeToIncidentEdges.values.flatten.toSet
		val coords2 = new scala.collection.mutable.ArrayBuffer[Point3f]
		ctEdges.foreach(e => {
			coords2 += positions(e.n1.vertex)
			coords2 += positions(e.n2.vertex)
		})
		
		println(coords.size)
		println(coords2.size)
		println(2*(ct.nodesContracted.size-1))
		
		lineArray = new LineArray(coords2.size, GeometryArray.COORDINATES | GeometryArray.COLOR_4)
		lineArray.setCoordinates(0, coords2.toArray)
		lineArray.setCapability(GeometryArray.ALLOW_COLOR_WRITE)
		val black = new Color4f
		lineArray.setColors(0, Array.fill(coords2.size)(black))

		val appearance = new Appearance()
		
		val lineAtts = new LineAttributes
		lineAtts.setLineAntialiasingEnable(true)
		lineAtts.setLineWidth(4f)
		appearance.setLineAttributes(lineAtts)
		
		val s3d = new Shape3D(lineArray)
		s3d.setAppearance(appearance)
		
		val tg = new TransformGroup()
		tg.addChild(s3d)
		
		objRoot.addChild(tg)
		*/
//		objRoot.compile()
		
		println("Updated contour tree panel.")
		
		simpleUniverse.addBranchGraph(objRoot)
		
		publish(new ContourTreeViewUpdatedEvent(lines, ct))
	}
	
	def getCoordsTopoSort(ct: ContourTree) = {
		val getIdx = ct.nodesContracted.zipWithIndex.toMap
		val g = new DefaultDirectedWeightedGraph[Int, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
		ct.nodesContracted.indices.foreach(g.addVertex(_))
		val ctEdges = ct.criticalNodeToIncidentEdges.values.flatten.toSet
		ctEdges.foreach(ctEdge => {
			val e = g.addEdge(getIdx(ctEdge.n1), getIdx(ctEdge.n2))
			g.setEdgeWeight(e, ctEdge.area)
		})
		
		val distMat = Array.fill[Double](ct.nodesContracted.size, ct.nodesContracted.size)(Double.MaxValue)
		ct.nodesContracted.indices.foreach(i => distMat(i)(i) = 0)
		val uf = new UnionFind(ct.nodesContracted.size)
		
		val predecessors = Array.fill(ct.nodesContracted.size)(List.empty[Int])

		val it = new TopologicalOrderIterator(g)
		while (it.hasNext) {
			val v = it.next
			g.incomingEdgesOf(v).map{e => 
				val parent = g.getEdgeSource(e)
				val w = g.getEdgeWeight(e)
				distMat(parent)(v) = w
				distMat(v)(parent) = w
				
				val parentUFID = uf.find(parent)
				predecessors(parentUFID).withFilter(_ != parent).foreach(pred => {
					
				})
			}
		}
		
	}
}

