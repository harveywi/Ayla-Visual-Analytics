package edu.osu.compgeom.ayla

import scala.swing._
import edu.osu.compgeom.ayla.menu._
import java.awt.Color
import javax.swing.OverlayLayout
import javax.swing.JLayeredPane
import java.net.URL
import org.artemis.progx.graphics.GraphicsUtilities
import java.awt.geom.AffineTransform
import javax.swing.BorderFactory
import javax.swing.ImageIcon
import scala.swing.event.UIElementHidden

class OpenProjectDialog(menu: PieMenu) extends Dialog {
  menu.mainFrame = this
  var chosenProject: Option[AylaCollaborationProjectDescriptor] = None
  title = "Open Ayla Collaboration Project"
  modal = true
  contents = new Component {
    preferredSize = new Dimension(800, 800)
    opaque = true
    background = Color.black
  }

  val textArea = new TextArea(0, 80) {
    editable = false
    wordWrap = true
    lineWrap = true
    text = "Hello"
    background = Color.black
    foreground = new Color(200, 200, 255)

    //    menu.menuAreas.filter(_.menuItem.userData.isInstanceOf[AylaCollaborationProjectDescriptor]).foreach{menuArea =>
    //      listenTo(menuArea.menuItem)
    //    }
    
    reactions += {
      case PieMenuItemHighlighted(item) => {
        text = item.userData.asInstanceOf[AylaCollaborationProjectDescriptor].description
      }
    }
  }
  
  listenTo(menu)
  reactions += {
    case UIElementHidden(`menu`) => {
      chosenProject = menu.selectedItem match {
        case Some(menuItem) => menuItem.userData.asInstanceOf[Option[AylaCollaborationProjectDescriptor]]
        case _ => None
      }
      close()
    }
  }

  val overlay = new BorderPanel {
    opaque = false
    //    add(new ScrollPane(textArea) {
    //      border = BorderFactory.createEmptyBorder()
    //      minimumSize = new Dimension(100, 50)
    //    }, BorderPanel.Position.North)
    add(menu, BorderPanel.Position.Center)
    add(new Component {
      val bgImage = GraphicsUtilities.loadCompatibleImage(Thread.currentThread().getContextClassLoader().getResource("logo_200h.png"))
      opaque = false
      preferredSize = new Dimension(0, bgImage.getHeight())
      override def paintComponent(g2d: Graphics2D): Unit = {
        g2d.setColor(Color.black)
        g2d.fillRect(0, 0, bounds.width, bounds.height)
        g2d.drawImage(bgImage, bounds.width / 2 - bgImage.getWidth() / 2, 0, null)
      }
    }, BorderPanel.Position.South)

    pack()
  }

  val layeredPane = peer.getRootPane.getLayeredPane
  layeredPane.setLayout(new OverlayLayout(layeredPane))
  layeredPane.add(overlay.peer, JLayeredPane.MODAL_LAYER)
  menu.visible = true
  centerOnScreen()
  open()
}

object OpenProjectDialog {
  def chooseProject(menu: PieMenu): Option[AylaCollaborationProjectDescriptor] = {
    val dialog = new OpenProjectDialog(menu)
    dialog.chosenProject
  }
}