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
import java.io.{ StringWriter, PrintWriter }
import java.awt.Font

class MessageDialog(messageText: String, details: String, dialogTitle: String) extends Dialog with ColorSchemed {
  modal = true
  title = dialogTitle

  contents = new BorderPanel with ColorSchemed {
    add(new TextArea with ColorSchemed {
      font = new Font("Verdana", Font.BOLD, 12)
      text = messageText
    }, BorderPanel.Position.North)

    if (details != "") {
      val textArea = new TextArea(details, 8, 80) with ColorSchemed {
        editable = false
      }
      add(new ScrollPane(textArea) with ColorSchemed, BorderPanel.Position.Center)
    }
  }

  centerOnScreen()
  pack()
  open()
}

object MessageDialog {
  def showErrorAndQuit(e: Exception) = {
    val errorDetails = {
      val stringWriter = new StringWriter
      e.printStackTrace(new PrintWriter(stringWriter))
      stringWriter.toString
    }
    val text = "A problem has occurred, and Ayla will now exit.  Please contact the developers.\nError details:"
    val dlg = new MessageDialog(text, errorDetails, "An error occurred")
    System.exit(0)
  }

  def showMessage(message: String, dialogTitle: String) = {
    new MessageDialog(message, "", dialogTitle)
  }
}
