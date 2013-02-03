/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui.desktop

import org.interactivemesh.scala.swing._
import ayla.client.ui.TerrainPanel
import scala.swing.event.UIElementResized
import java.awt.Image
import java.awt.Robot
import java.awt.Toolkit
import java.awt.Rectangle
import com.sun.awt.AWTUtilities
import javax.swing.RepaintManager
import scala.collection.mutable.ArrayBuffer
import javax.swing.JLayeredPane
import ayla.client.ui.event._
import ayla.collab.ConformationAnnotation
import javax.vecmath.Point3f
import javax.vecmath._
import javax.media.j3d.Transform3D
import java.awt.geom.Ellipse2D
import scala.swing.event.MouseMoved

class DesktopPane(val terrainPanel: TerrainPanel) extends InternalDesktopPane {
  desktopManager = new InternalDesktopManager(this, terrainPanel)
  dragMode = InternalDesktopPane.DragMode.Live

  val jmolPalettes = new ArrayBuffer[JmolPalette]

  terrainPanel.canvas3D.repaint()

  import LayeredPane._
  layout(terrainPanel) = new LayerConstraints(layer = 1) // background
  jmolPalettes.foreach(layout(_) = new LayerConstraints(layer = 3))

  listenTo(this)
  reactions += {
    case UIElementResized(desktopPane) => {
      val size = desktopPane.size
      setupLayeredPane
    }

    case AnnotationsRefreshed(newAnnotations) => {
      // Only create new JmolThumbnails for those conformations which don't already have a palette
      val curPaletteSet = jmolPalettes.map(palette => (palette.annotation -> palette)).toMap
      newAnnotations.foreach { a =>
        curPaletteSet.get(a) match {
          case Some(palette) => {}
          case None => {
//            terrainPanel.floorplan.
            val newJmolPalette = new JmolPalette(this, a, terrainPanel)
            listenTo(newJmolPalette)
            javax.swing.ToolTipManager.sharedInstance.setLightWeightPopupEnabled(false)
            jmolPalettes += newJmolPalette
            layout(newJmolPalette) = new LayerConstraints(layer = 3)
          }
        }
      }
      setupLayeredPane
    }
    
    case RefreshAnnotationVisibilities => {
      jmolPalettes.foreach(palette => palette.visible = palette.annotation.visible)
      terrainPanel.repaint()
    }
    
    case e: JmolPaletteMoved => {
      if (e.src != this)
        publish(new JmolPaletteMoved(this, e.jmolPalette, e.newLocation))
    }
    
    case TerrainPanelClicked(p) => {
      jmolPalettes.filter(_.visible).find{palette => 
      	val p3f = palette.getTerrainVert
      	val p2d = get3dTo2dPoint(new Point3d(p3f.x, p3f.y, p3f.z))
      	val r = 6
      	val oval = new Ellipse2D.Double(p2d.x.toInt - r, p2d.y.toInt - r, 2*r, 2*r)
      	oval.contains(p.x, p.y)
      } match {
        case Some(palette) => {
          palette.isChangingOvalLocation = !palette.isChangingOvalLocation
        }
        case None => {
          jmolPalettes.find(_.isChangingOvalLocation) match {
            case Some(palette) => {
              val mouseCoords = new Point2d(terrainPanel.mouseX, terrainPanel.mouseY)
              val nearestIdx = palette.terrainVerts.zipWithIndex.minBy {
                case (vert, idx) =>
                  val p = vert.p
                  val p2d = get3dTo2dPoint(new Point3d(p.x, p.y, p.z))
                  mouseCoords.distanceSquared(p2d)
              }._2
              palette.terrainVertexID = nearestIdx
              palette.isChangingOvalLocation = !palette.isChangingOvalLocation
            }
            case None => {}
          }
        }
      }
    }
  }
  
    def get3dTo2dPoint(point3d: Point3d): Point2d = {
      val temp = new Transform3D();
      terrainPanel.canvas3D.getVworldToImagePlate(temp);
      temp.transform(point3d);
      val point2d = new Point2d();
      terrainPanel.canvas3D.getPixelLocationFromImagePlate(point3d, point2d);
      return point2d;
    }
  
  def setupLayeredPane: Unit = {

    val size = this.size

    val iframesHeight = size.height

    val frameX = 0
    val frameY = 0

    desktopManager.setBoundsForFrame(terrainPanel, 0, 0, size.width, size.height)

    jmolPalettes.foreach(jmolPalette => {
      val paletteWidth = jmolPalette.preferredSize.width
      val paletteHeight = jmolPalette.preferredSize.height
      desktopManager.setBoundsForFrame(jmolPalette, size.width - paletteWidth, size.height - paletteHeight, paletteWidth, paletteHeight)
    })

    this.revalidate()
    this.repaint()
  }
}
