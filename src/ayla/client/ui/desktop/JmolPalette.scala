/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui.desktop

import scala.swing._
import scala.swing.event._
import org.interactivemesh.scala.swing._
import javax.swing.plaf.basic.BasicInternalFrameUI
import java.awt.event.MouseListener
import java.awt.event.MouseEvent
import java.awt.Color
import java.awt.geom.RoundRectangle2D
import java.awt.RenderingHints
import javax.swing.plaf.metal.MetalInternalFrameUI
import javax.swing.JInternalFrame
import java.awt.image.BufferedImage
import java.awt.Robot
import java.awt.Toolkit
import javax.swing.SwingUtilities
import javax.swing.RepaintManager
import com.sun.awt.AWTUtilities
import ayla.client.ui.JmolPanel
import javax.swing.BorderFactory
import java.io.File
import ayla.collab.ConformationAnnotation
import javax.vecmath.Point3f
import scala.collection.mutable.ArrayBuffer
import ayla.landscape._
import ayla.client.ui.TerrainPanel
import ayla.client.ui.ColorSchemes

case class JmolPaletteMoved(src: Component, val jmolPalette: JmolPalette, newLocation: Point) extends Event

class JmolPalette(desktopPane: DesktopPane, val annotation: ConformationAnnotation, terrainPanel: TerrainPanel) extends BorderPanel {
  var terrainVertexID: Int = 0
  var terrainVerts: ArrayBuffer[_ <: TerrainPos] = null
  
  var isChangingOvalLocation = false

  def getTerrainVert: Point3f = {
    val floorplan = terrainPanel.floorplan
    if (floorplan == null) {
      new Point3f
    } else {
      if (terrainVerts == null) {
        val terrainVertsOpt = floorplan.contourTreeCriticalNodeToTerrainVerts.get(annotation.sampledConformationID) match {
          case Some(verts) => {
            //val tv = terrainVerts.head
            //val tv = verts.map(floorplan.terrainVertices)
            val tv = verts.map(i => new CriticalPointTerrainPos(i, floorplan.terrainVertices(i)))
            //        Some(new Point3f(floorplan.terrainVertices(tv).x, floorplan.terrainVertices(tv).y, floorplan.terrainVertices(tv).z))
            Some(tv)
          }
          case None => {
            //floorplan.contourTreeNoncriticalNodesToTerrainVerts.get(annotation.sampledConformationID)
            floorplan.contourTreeNoncriticalNodesToTerrainVerts.get(annotation.sampledConformationID) match {
              case Some(verts) => {
                Some(verts)
              }
              case None => None
            }
          }
        }
        terrainVerts = terrainVertsOpt.getOrElse(scala.collection.mutable.ArrayBuffer(new CriticalPointTerrainPos(0, new Point3f)))
      }
      terrainVerts(terrainVertexID).p
    }
  } //terrainVerts(terrainVertexID)

  val jmolPanel = new JmolPanel {
    override def paint(g: java.awt.Graphics): Unit = {
      super.paint(g)
      g.setColor(ColorSchemes.scheme.bgColor)
      g.drawRect(0, 0, getWidth - 1, getHeight - 1)
    }
  }

  val dragDoodad = new BorderPanel {
    listenTo(mouse.moves)
    listenTo(mouse.clicks)
    listenTo(mouse.wheel)
    background = new Color(73, 96, 112)
    preferredSize = new Dimension(0, 10)
    visible = false

    val dragStart = new Point()

    reactions += {
      case e: MouseEntered => {
        visible = true
      }
      case e: MouseExited => {
        visible = false
      }
      case e: MousePressed => dragStart.setLocation(e.point)
      case e: MouseDragged => {
        val dx = e.point.x - dragStart.x
        val dy = e.point.y - dragStart.y
        val oldLocation = JmolPalette.this.location
        JmolPalette.this.peer.setLocation(new Point(oldLocation.x + dx, oldLocation.y + dy))
      }
      case e: MouseWheelMoved => {
        terrainVertexID = math.max(0, (terrainVerts.size + terrainVertexID + e.rotation) % terrainVerts.size)
        println(e.rotation)
      }
    }
  }

  add(dragDoodad, BorderPanel.Position.North)

  add(new Component {
    override lazy val peer = jmolPanel
    listenTo(mouse.moves)
    listenTo(mouse.clicks)

    reactions += {
      case e: MouseClicked => {
        if (e.peer.getButton() == java.awt.event.MouseEvent.BUTTON3) {
          // Do nothing to prevent the Jmol menu from appearing - it is glitchy since
          // we are mixing heavyweight and lightweight components
          //          e.consume()
        }
      }
      case e: MouseEntered => {
        println("Entered.")
        dragDoodad.visible = true
      }
      case e: MouseExited => {
        println("Exited.")
        dragDoodad.visible = false
      }
    }
  }, BorderPanel.Position.Center)

  border = null

  preferredSize = new Dimension(100, 200)
  visible = true
  jmolPanel.showConformation(annotation.pdbLines)

  listenTo(this)
  reactions += {
    case e: UIElementMoved => {
      val cx = location.x + bounds.width / 2
      val cy = location.y + bounds.height / 2
      publish(new JmolPaletteMoved(this, this, new Point(cx, cy)))
    }

  }
}
