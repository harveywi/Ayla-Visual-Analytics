/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.collab

import scala.swing._
import scala.swing.event._
import ayla.client.ui.ColorSchemes.scheme
import org.artemis.progx.graphics.GraphicsUtilities
import javax.swing.ImageIcon
import ConformationAnnotationListItem._

class ConformationAnnotationListItem(val annotation: ConformationAnnotation) extends BorderPanel {
  background = scheme.bgColor
  foreground = scheme.btnForeground
  
  def selected = checkBtn.selected
  def selected_= = checkBtn.selected_=(_)

  val checkBtn = new Button {
    selected = annotation.visible
    icon = uncheckedIcon
    selectedIcon = checkedIcon
    background = scheme.bgColor
    foreground = scheme.btnForeground
    borderPainted = false
    
    reactions += {
      case e: ButtonClicked => {
      	selected = !selected
      }
    }
  }
  private val label = new Label("%s [%s]".format(annotation.name, annotation.timestamp)) {
    background = scheme.bgColor
    foreground = scheme.btnForeground
    xAlignment = Alignment.Left
  }

  add(checkBtn, BorderPanel.Position.West)
  add(label, BorderPanel.Position.Center)
}

object ConformationAnnotationListItem {
  val uncheckedIcon = {
    val img = GraphicsUtilities.createCompatibleImage(13, 13)
    val g = img.createGraphics
    g.setColor(scheme.btnBackground)
    g.fillRect(0, 0, img.getWidth, img.getHeight)
    g.setColor(scheme.lineColor)
    g.drawRect(0, 0, img.getWidth-1, img.getHeight-1)
    new ImageIcon(img)
  }

  val checkedIcon = {
    val img = GraphicsUtilities.createCompatibleImage(13, 13)
    val g = img.createGraphics
    g.setColor(scheme.btnForeground)
    g.fillRect(0, 0, img.getWidth, img.getHeight)
    g.setColor(scheme.lineColor)
    g.drawRect(0, 0, img.getWidth-1, img.getHeight-1)
    new ImageIcon(img)
  }

}
