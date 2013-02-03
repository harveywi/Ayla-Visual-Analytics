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
import scala.swing.Component
import ayla.client.ui.TerrainPanel

class InternalDesktopManager(desktopPane: InternalDesktopPane, terrainPanel: TerrainPanel) extends InternalDefaultDesktopManager {
  // Keep Component within the area of desktop
  override def dragFrame(c: Component, newX: Int, newY: Int) {
    var x = newX
    var y = newY

    if (x < 0) x = 0
    else {
      if (x + c.size.width > desktopPane.size.width) {
        x = desktopPane.size.width - c.size.width
      }
    }

    if (y < 0) y = 0
    else {
      if (y + c.size.height > desktopPane.size.height) {
        y = desktopPane.size.height - c.size.height
      }
    }

    super.dragFrame(c, x, y)
  }
  
  override def setBoundsForFrame(c: Component, a: Int, b: Int, p: Int, q: Int) = {
    desktopPane.revalidate()
    super.setBoundsForFrame(c, a, b, p, q)
  }

  override def deiconifyFrame(f: InternalFrame) {
    if (f.maximum) {
      // Stop Java 3D rendering for non-visible canvas3ds
      terrainPanel.canvas3D.stopRenderer

      //        popupMenu.frameMaximum(true)
    }

    super.deiconifyFrame(f)
  }

  override def iconifyFrame(f: InternalFrame) {
    if (f.maximum) {
      // Start Java 3D rendering for all canvas3ds
      terrainPanel.canvas3D.startRenderer

      //        popupMenu.frameMaximum(false)
    }

    super.iconifyFrame(f)
  }

  override def maximizeFrame(f: InternalFrame) {

    //      popupMenu.frameMaximum(true)

    super.maximizeFrame(f)

    // Stop Java 3D rendering for non-visible canvas3ds
    terrainPanel.canvas3D.stopRenderer
  }

  override def minimizeFrame(f: InternalFrame) {

    // Start Java 3D rendering for all canvas3ds
    terrainPanel.canvas3D.startRenderer

    super.minimizeFrame(f)

    //      popupMenu.frameMaximum(false)
  }
}
