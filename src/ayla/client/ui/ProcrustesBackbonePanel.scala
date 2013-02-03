/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui

import javax.media.j3d._
import javax.vecmath._
import com.sun.j3d.utils.universe._
import com.sun.j3d.utils.image._
import org.interactivemesh.scala.swing.j3d.SCanvas3D
import com.sun.j3d.utils.behaviors.vp.OrbitBehavior
import java.awt.GraphicsEnvironment
import java.awt.GraphicsConfigTemplate
import java.awt.GraphicsConfiguration
import java.awt.GraphicsDevice
import com.sun.j3d.utils.geometry.Box
import com.sun.j3d.utils.geometry.ColorCube
import scala.swing.event._
import no.uib.cipr.matrix.DenseMatrix
import no.uib.cipr.matrix.SVD
import no.uib.cipr.matrix.sparse.CompDiagMatrix
import com.sun.j3d.utils.behaviors.mouse.MouseWheelZoom
import com.sun.j3d.utils.behaviors.mouse.MouseRotate
import java.io._
import ayla.colormap._
import java.awt.Color
import ayla.util.IO._
import ayla.j3d.MySJCanvas3D
import java.awt.AWTEvent

class ProcrustesBackbonePanel extends MySJCanvas3D(new GraphicsConfigTemplate3D {
      setSceneAntialiasing(GraphicsConfigTemplate.PREFERRED)
    }, (c: Canvas3D) => {}) {
  
  listenTo(this)
  enableWakeupOnAWTEvents(AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_MOTION_EVENT_MASK | AWTEvent.MOUSE_WHEEL_EVENT_MASK)
  
  val canvas3d = this.offscreenCanvas3D

  def getCarbonBackbone(pdbFile: InputStream): Array[Point3f] = {

    withBufferedReader(pdbFile) { br =>
      val carbonCoords = new scala.collection.mutable.ArrayBuffer[Point3f]()

      while (br.ready) {
        val line = br.readLine
        if (line.startsWith("ATOM")) {
          val atomType = line.substring(11, 17).trim
          if (atomType == "CA") {
            // ATOM   3602  C   THR   226      20.219   7.787  26.920  0.00  0.00
            val x = line.substring(26, 38).toFloat
            val y = line.substring(38, 46).toFloat
            val z = line.substring(46, 54).toFloat
            val p = new Point3f(x, y, z)
            carbonCoords += p
          }
        }
      }
      br.close
      carbonCoords.toArray
    }
  }

  //  listenTo(this)
  //  listenTo(mouse.clicks)
  listenTo(mouse.wheel)
  //  listenTo(mouse.moves)
  //  

  var zoomTransformGroup = new TransformGroup {
    setCapability(TransformGroup.ALLOW_TRANSFORM_READ)
    setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
  }

  /*
  reactions += {
    case e: MouseWheelMoved => {
      val t = new Transform3D
      zoomTransformGroup.getTransform(t)
      val scale = t.getScale
      val zoomFactor = .18
      if (e.rotation == 1)
      	t.setScale(scale + scale*zoomFactor)
      else
        t.setScale(scale - scale*zoomFactor)
      zoomTransformGroup.setTransform(t)
    }
  }
  */

  var simpleUniverse = new SimpleUniverse(canvas3d)
  var view = simpleUniverse.getViewer().getView()
  view.setMinimumFrameCycleTime(17)
  view.setFrontClipDistance(.01)
  view.setSceneAntialiasingEnable(true)
  var viewingPlatform = simpleUniverse.getViewingPlatform()
  viewingPlatform.setNominalViewingTransform()

  val tg = new TransformGroup
  tg.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
  //	orbit.setProportionalZoom(true)
  //	orbit.setZoomFactor(.18)
  //	
  //	val vpExecutor = new VantagePointBehavior(orbit); 
  //  vpExecutor.setSchedulingBounds(new BoundingSphere(new Point3d(), 1000.0));

  //	viewingPlatform.setViewPlatformBehavior(new VantagePointBehavior(orbit))
  /*
	val orbit = new OrbitBehavior(this.offscreenCanvas3D, OrbitBehavior.REVERSE_ALL)
	orbit.setProportionalZoom(true)
	orbit.setZoomFactor(.18)
	val mouseBounds = new BoundingSphere(new Point3d(), 1000.0)
	orbit.setSchedulingBounds(mouseBounds)
	viewingPlatform.setViewPlatformBehavior(orbit)
	*/

  var objRoot = new BranchGroup()
  objRoot.setCapability(BranchGroup.ALLOW_DETACH)
  objRoot.setCapability(Group.ALLOW_CHILDREN_WRITE)
  objRoot.setCapability(Group.ALLOW_CHILDREN_EXTEND)

  val bg = new Background(new Color3f(ColorSchemes.scheme.bgColor))
  bg.setApplicationBounds(new BoundingSphere(new Point3d(), 1000.0))
  objRoot.addChild(bg)

  simpleUniverse.addBranchGraph(objRoot)

  def setSelectedConformations(pdbFilesIn: Array[InputStream]): Unit = {
    val pdbFiles = pdbFilesIn //.take(50)
    if (objRoot != null) {
      objRoot.detach()
    }

    objRoot = new BranchGroup()
    objRoot.setCapability(BranchGroup.ALLOW_DETACH)
    objRoot.setCapability(Group.ALLOW_CHILDREN_WRITE)
    objRoot.setCapability(Group.ALLOW_CHILDREN_EXTEND)

    val bg = new Background(new Color3f(ColorSchemes.scheme.bgColor))
    bg.setApplicationBounds(new BoundingSphere(new Point3d(), 1000.0))
    objRoot.addChild(bg)

    if (pdbFiles.size == 0) {
      objRoot.compile
      simpleUniverse.addBranchGraph(objRoot)
      return
    }

    val spin = new TransformGroup
    spin.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
    val spinAlpha = new Alpha(-1, 10000)
    val rotInterp = new RotationInterpolator(spinAlpha, spin)
    rotInterp.setSchedulingBounds(new BoundingSphere(new Point3d(), 1000.0))
    spin.addChild(rotInterp)

    zoomTransformGroup = new TransformGroup
    zoomTransformGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_READ)
    zoomTransformGroup.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
    val trans = new Transform3D
    trans.setScale(.02)
    zoomTransformGroup.setTransform(trans)
    zoomTransformGroup.addChild(spin)

    val mouseWheelZoom = new MouseWheelZoom(this.peer, zoomTransformGroup)
    mouseWheelZoom.setFactor(.18)
    mouseWheelZoom.setSchedulingBounds(new BoundingSphere(new Point3d(), 1000.0))
    zoomTransformGroup.addChild(mouseWheelZoom)

    //	  val mouseRotate = new MouseRotate(this.peer, zoomTransformGroup)
    //	  mouseRotate.setSchedulingBounds(new BoundingSphere(new Point3d(), 1000.0))
    //	  zoomTransformGroup.addChild(mouseRotate)

    objRoot.addChild(zoomTransformGroup)

    println("Calculating conformation points")
    val backboneBG = new BranchGroup
    spin.addChild(backboneBG)

    val backbones = pdbFiles.map(getCarbonBackbone(_))

    def getMean(backbone: Array[Point3f]): Point3f = {
      val mean = new Point3f
      backbone.foreach(p => {
        mean.add(p)
      })

      mean.x /= backbone.size.toFloat
      mean.y /= backbone.size.toFloat
      mean.z /= backbone.size.toFloat
      mean
    }

    val target = backbones(0).map(p => new Point3f(p.x, p.y, p.z))
    val targetMean = getMean(target)
    target.foreach(p => {
      p.sub(targetMean)
    })

    val B = new DenseMatrix(target.size, 3)
    target.indices.foreach(i => {
      val p = target(i)
      B.set(i, 0, p.x)
      B.set(i, 1, p.y)
      B.set(i, 2, p.z)
    })

    println("Target centroid:  " + getMean(target))

    val A = new DenseMatrix(3, target.size)
    val Anew = new DenseMatrix(3, target.size)
    val cov = new DenseMatrix(3, 3)
    val S1 = new CompDiagMatrix(3, 3, Array(0))
    S1.set(0, 0, 1)
    S1.set(1, 1, 1)
    S1.set(2, 2, -1)
    val S2 = new CompDiagMatrix(3, 3, Array(0))
    S2.set(0, 0, 1)
    S2.set(1, 1, 1)
    S2.set(2, 2, 1)
    val R = new DenseMatrix(3, 3)

    def covDet: Double = {
      val a = cov.get(0, 0)
      val b = cov.get(1, 0)
      val c = cov.get(2, 0)
      val d = cov.get(0, 1)
      val e = cov.get(1, 1)
      val f = cov.get(2, 1)
      val g = cov.get(0, 2)
      val h = cov.get(1, 2)
      val i = cov.get(2, 2)
      return a * e * i + b * f * g + c * d * h - a * f * h - b * d * i - c * e * g
    }

    (0 until 2).foreach(_ => {
      backbones.foreach(backbone => {
        val center = getMean(backbone)
        backbone.foreach(p => p.sub(center))

        backbone.indices.foreach(i => {
          val p = backbone(i)
          A.set(0, i, p.x)
          A.set(1, i, p.y)
          A.set(2, i, p.z)
        })

        A.mult(B, cov)
        val det = covDet

        val svd = SVD.factorize(cov)

        //		    if (det > 0) {
        //		      S.set(0, 0, 1)
        //		      S.set(1, 1, 1)
        //		      S.set(2, 2, -1)
        //		    } else {
        //		      S.set(0, 0, 1)
        //		      S.set(1, 1, 1)
        //		      S.set(2, 2, 1)
        //		    }

        var bestSSD = Double.PositiveInfinity
        List(S1, S2).foreach(S => {
          val temp = new DenseMatrix(3, 3)
          svd.getU().mult(S, temp)
          temp.mult(svd.getVt(), R)
          R.mult(A, Anew)

          val p = new Point3f
          val ssd = backbone.indices.map(i => {
            p.x = Anew.get(0, i).toFloat
            p.y = Anew.get(1, i).toFloat
            p.z = Anew.get(2, i).toFloat
            target(i).distanceSquared(p)
          }).sum

          if (ssd < bestSSD) {
            bestSSD = ssd
            backbone.indices.foreach(i => {
              val p = backbone(i)
              p.x = Anew.get(0, i).toFloat
              p.y = Anew.get(1, i).toFloat
              p.z = Anew.get(2, i).toFloat
            })
          }
        })
      })

      // Now all of the backbones are aligned with the current target.  Recompute the
      // target as the mean of all of the backbones.
      target.indices.foreach(i => target(i).set(0, 0, 0))
      backbones.foreach(backbone => {
        target.indices.foreach(i => target(i).add(backbone(i)))
      })
      target.foreach(p => {
        p.x /= backbones.size.toFloat
        p.y /= backbones.size.toFloat
        p.z /= backbones.size.toFloat
      })

      target.indices.foreach(i => {
        val p = target(i)
        B.set(i, 0, p.x)
        B.set(i, 1, p.y)
        B.set(i, 2, p.z)
      })
    })

    // Build a colormap which shows each backbone's SSD distance from the target
    val allRMSDs = backbones.map { backbone =>
      val ssd = backbone.indices.map(i => {
        backbone(i).distanceSquared(target(i))
      }).sum
      math.sqrt(ssd / backbone.size.toFloat)
    }

    val cmap = new DivergingColormap(new Color(0, 0, 255), new Color(160, 160, 255))
    val minRMSD = allRMSDs.min
    val rangeRMSD = allRMSDs.max - minRMSD
    def getColor(rmsd: Double) = new Color3f(cmap.interpolateDiverging((rmsd - minRMSD) / rangeRMSD))

    backbones.zip(allRMSDs).foreach {
      case (backbone, rmsd) => {
        val meanDist = (0 until backbone.size - 1).map(i => {
          val p1 = backbone(i)
          val p2 = backbone(i + 1)
          p1.distance(p2)
        }).sum / (backbone.size - 1).toDouble

        val verts = (0 until backbone.size - 1).flatMap(i => {
          val p1 = backbone(i)
          val p2 = backbone(i + 1)
          if (p1.distance(p2) < 3 * meanDist)
            Array(p1, p2)
          else
            Array.empty[Point3f]
        })

        val lineArray = new LineArray(verts.size, GeometryArray.COORDINATES)
        lineArray.setCoordinates(0, verts.toArray)

        val color = getColor(rmsd)

        val appearance = new Appearance()

        val lineAtts = new LineAttributes
        lineAtts.setLineAntialiasingEnable(true)
        lineAtts.setLineWidth(1f)
        appearance.setLineAttributes(lineAtts)

        val colorAtts = new ColoringAttributes
        colorAtts.setColor(color)
        appearance.setColoringAttributes(colorAtts)

        val s3d = new Shape3D(lineArray)
        s3d.setAppearance(appearance)
        backboneBG.addChild(s3d)
      }
    }
    objRoot.compile

    simpleUniverse.addBranchGraph(objRoot)
  }

}

object ProcrustesBackbonePanel {
  def apply() = {
    //    val device = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
    //    val template = new GraphicsConfigTemplate3D {
    //			setSceneAntialiasing(GraphicsConfigTemplate.PREFERRED)
    //		}

    new ProcrustesBackbonePanel()
  }

}
