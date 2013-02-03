/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui

import scala.swing._
import java.awt.Font
import scala.swing.event.ButtonClicked

class ConfirmationDialog(dlgTitle: String, dlgText: String, choice1: String, choice2: String) extends Dialog with ColorSchemed {
  var result: String = choice1

  title = dlgTitle
  modal = true

  val btn1 = new Button(choice1) with ColorSchemed {
    reactions += {
      case e: ButtonClicked => 
        result = choice1 
        close()
    }
  }
  val btn2 = new Button(choice2) with ColorSchemed {
    reactions += {
      case e: ButtonClicked =>
        result = choice2
        close()
    }
  }

  contents = new BorderPanel with ColorSchemed {
    add(new Label with ColorSchemed {
      text = dlgText
    }, BorderPanel.Position.Center)

    add(new FlowPanel with ColorSchemed {
      contents ++= Seq(btn1, btn2)
    }, BorderPanel.Position.South)
  }

  centerOnScreen()
  open()
}

object ConfirmationDialog {
  def getChoice(title: String, text: String, choice1: String, choice2: String): String = {
    val dlg = new ConfirmationDialog(title, text, choice1, choice2)
    dlg.result
  }
}
