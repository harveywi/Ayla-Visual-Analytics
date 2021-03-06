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
import java.util.prefs._
import javax.swing.BorderFactory
import scala.swing.event._
import java.io.File
import ayla.client.ui.state.UIState
import java.net.Socket
import java.net.InetAddress
import java.net.ConnectException
import ayla.client.ui.event.ColorSchemeChanged

class LoginDialog extends Dialog with ColorSchemed {
  var result: Option[LoginInfo] = None

  private[this] val colorSchemeComboBox = new ComboBox(Seq("Dark", "Light")) with ColorSchemed {
    reactions += {
      case e: SelectionChanged => selection.item match {
        case "Dark" =>
          UIState.colorScheme = ColorSchemes.dark
        case "Light" =>
          UIState.colorScheme = ColorSchemes.light
      }
    }
  }

  title = "Connect to Ayla Server"
  modal = true
  private[this] val prefs = Preferences.userRoot().node("/edu/osu/compgeom/ayla")

  private[this] def makeTextField(defaultText: String): TextField = new TextField(30) with LineBordered {
    listenTo(this)
    listenTo(keys)
    focusable = true
    text = defaultText

    reactions += {
      case e: FocusGained => selectAll()
      case e: FocusLost => peer.select(0, 0)
      case KeyPressed(_, Key.Enter, _, _) => {
        connectButton.doClick()
      }
    }
  }

  private[this] val serverTextField = makeTextField(prefs.get("servername", "thor.cse.ohio-state.edu"))
  private[this] val usernameTextField = makeTextField(prefs.get("username", "[your username]"))
  //  private[this] val cacheDirTextField = makeTextField(prefs.get("cacheDir", new File(".").getAbsolutePath))
  private[this] val connectButton = new Button("Connect") with ColorSchemed {
    focusable = true

    reactions += {
      case e: ButtonClicked =>
        prefs.put("servername", serverTextField.text)
        prefs.put("username", usernameTextField.text)

        // Make sure internet address/socket is reachable
        try {
          val ia = InetAddress.getByName(serverTextField.text)
          val socket = new Socket(ia, 9010)

          result = Some(LoginInfo(serverTextField.text, usernameTextField.text, socket))
          close()
        } catch {
          case e: ConnectException =>
            Dialog.showMessage(message = s"Unable to connect to server ${serverTextField.text} on port 9010.", title = "Connection Refused")
        }
    }
  }

  defaultButton = connectButton

  contents = new GridBagPanel with ColorSchemed {
    val c = new Constraints
    c.insets = new Insets(2, 2, 2, 2)

    def makeLabel(text: String) = new Label(text) with ColorSchemed { horizontalAlignment = Alignment.Right }

    val lblServer = makeLabel("Server:  ")
    c.fill = GridBagPanel.Fill.Horizontal
    c.gridx = 0
    c.gridy = 0
    layout(lblServer) = c

    c.gridx = 1
    layout(serverTextField) = c

    val lblUserName = makeLabel("Username:  ")
    c.gridx = 0
    c.gridy = 1
    layout(lblUserName) = c

    c.gridx = 1
    layout(usernameTextField) = c

//    val lblColorScheme = makeLabel("Color scheme:  ")
//    c.gridx = 0
//    c.gridy = 2
//    layout(lblColorScheme) = c
//
//    c.gridx = 1
//    layout(colorSchemeComboBox) = c

    c.gridx = 0
    c.gridy = 2
    c.gridwidth = 2
    c.fill = GridBagPanel.Fill.None
    layout(connectButton) = c
  }

  centerOnScreen()
  open()

}

case class LoginInfo(val server: String, val username: String, val socket: Socket)

object LoginDialog {
  def main(args: Array[String]): Unit = {
    println("Got result:  " + getLoginInfo())
    System.exit(0)
  }

  def getLoginInfo(): Option[LoginInfo] = {
    val dialog = new LoginDialog
    dialog.close()
    dialog.result
  }
}
