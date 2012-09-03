package edu.osu.compgeom.ayla

import scala.swing._
import scala.swing.event._
import org.artemis.progx.graphics.GraphicsUtilities
import javax.swing.ImageIcon
import java.awt.Color
import edu.osu.compgeom.omegavis.ColorSchemes.scheme
import javax.swing.BorderFactory
import java.util.prefs._
import java.io.File

class LoginDialog extends Dialog {
  title = "Connect to Ayla Server"
  modal = true
  
  val prefs = Preferences.userRoot().node("/edu/osu/compgeom/ayla")
  
  background = scheme.bgColor

  var result: Option[LoginInfo] = None

  val serverTextField = new TextField(30) {
    listenTo(this)
    listenTo(keys)
    focusable = true
    text = prefs.get("servername", "thor.cse.ohio-state.edu")
    background = scheme.bgColor
    foreground = scheme.btnForeground
    border = BorderFactory.createLineBorder(scheme.lineColor)
    
    reactions += {
      case e: FocusGained => selectAll()
      case e: FocusLost => peer.select(0, 0)
      case KeyPressed(_, Key.Enter, _, _) => {
        connectButton.doClick()
      }
    }
  }
  val usernameTextField = new TextField {
    listenTo(this)
    listenTo(keys)
    focusable = true
    
    text = prefs.get("username", "[your username]")
    background = scheme.bgColor
    foreground = scheme.btnForeground
    border = BorderFactory.createLineBorder(scheme.lineColor)
    
    reactions += {
      case e: FocusGained => selectAll()
      case e: FocusLost => peer.select(0, 0)
      case KeyPressed(_, Key.Enter, _, _) => {
        connectButton.doClick()
      }
    }
  }
  
  val cacheDirTextField = new TextField {
    listenTo(this)
    listenTo(keys)
    focusable = true
    
    text = prefs.get("cacheDir", new File(".").getAbsolutePath)
    background = scheme.bgColor
    foreground = scheme.btnForeground
    border = BorderFactory.createLineBorder(scheme.lineColor)
    
    reactions += {
      case e: FocusGained => selectAll()
      case e: FocusLost => peer.select(0, 0)
      case KeyPressed(_, Key.Enter, _, _) => {
        connectButton.doClick()
      }
    }
  }
  
  
  val connectButton: Button = new Button("Connect") {
    focusable = true
    background = scheme.bgColor
    foreground = scheme.btnForeground
    reactions += {
      case e: ButtonClicked => {
        prefs.put("servername", serverTextField.text)
        prefs.put("username", usernameTextField.text)
        prefs.put("cacheDir", cacheDirTextField.text)
        
        // Make sure cache directory exists
        val cacheDir = new File(cacheDirTextField.text)
        if (!cacheDir.exists) {
          Dialog.showConfirmation(null, "The specified cache directory does not exist.  Would you like me to create it?", "Cache Directory Not Found") match {
            case Dialog.Result.Yes => {
              cacheDir.mkdirs()
              result = Some(LoginInfo(serverTextField.text, usernameTextField.text, cacheDir))
              close()
            }
            case Dialog.Result.No => {}
          }
        } else {
          // Cache found
          result = Some(LoginInfo(serverTextField.text, usernameTextField.text, cacheDir))
          close()
        }
      }
    }
  }
  
//  val cachePathField = new TextField {
//    listenTo(this)
//    listenTo(keys)
//    focusable = true
//    text = "[
//  }
  
  defaultButton = connectButton
  
  contents = new GridBagPanel {
    background = scheme.bgColor
    foreground = scheme.btnForeground
    
    val c = new Constraints
    c.insets = new Insets(2, 2, 2, 2)
    
    val lblServer = new Label("Server:  ") {
      background = scheme.btnBackground
      foreground = scheme.btnForeground
      horizontalAlignment = Alignment.Right
    }
    c.fill = GridBagPanel.Fill.Horizontal
    c.gridx = 0
    c.gridy = 0
    layout(lblServer) = c
    
    c.gridx = 1
    layout(serverTextField) = c
    
    val lblUserName = new Label("Username:  ") {
      background = scheme.btnBackground
      foreground = scheme.btnForeground
      horizontalAlignment = Alignment.Right
    }
    c.gridx = 0
    c.gridy = 1
    layout(lblUserName) = c
    
    c.gridx = 1
    layout(usernameTextField) = c
    
    val lblCacheDir = new Label("Cache Directory:  ") {
      background = scheme.btnBackground
      foreground = scheme.btnForeground
      horizontalAlignment = Alignment.Right
    }
    c.gridx = 0
    c.gridy = 2
    layout(lblCacheDir) = c
    
    c.gridx = 1
    layout(cacheDirTextField) = c
    
    
    c.gridx = 0
    c.gridy = 3
    c.gridwidth = 2
    c.fill = GridBagPanel.Fill.None
    layout(connectButton) = c
  }

//  contents = new BorderPanel {
//    add(new BorderPanel {
//      background = scheme.btnBackground;
//      add(new BorderPanel {
//        background = scheme.btnBackground; foreground = scheme.btnForeground;
//        add(new GridPanel(0, 1) {
//          background = scheme.btnBackground; foreground = scheme.btnForeground;
//          contents += new Label("Server:  ") { background = scheme.btnBackground; foreground = scheme.btnForeground; horizontalAlignment = Alignment.Right }
//          contents += new Label("User Name:  ") { background = scheme.btnBackground; foreground = scheme.btnForeground; horizontalAlignment = Alignment.Right }
//        }, BorderPanel.Position.Center)
//        add(new GridPanel(0, 1) {
//          background = scheme.btnBackground; foreground = scheme.btnForeground;
//          contents ++= List(serverTextField, usernameTextField)
//        }, BorderPanel.Position.East)
//      }, BorderPanel.Position.North)
//
//      add(new BorderPanel { background = scheme.btnBackground; foreground = scheme.btnForeground; add(connectButton, BorderPanel.Position.East) }, BorderPanel.Position.Center)
//    }, BorderPanel.Position.North)
//    add(new BorderPanel {background = scheme.btnBackground;}, BorderPanel.Position.Center)
//  }

  centerOnScreen()
  open()
}

case class LoginInfo(val server: String, val username: String, val cacheDir: File)

object LoginDialog {
  def main(args: Array[String]): Unit = {
    println("Got result:  " + getLoginInfo())
    System.exit(0)
  }

  def getLoginInfo(): Option[LoginInfo] = {
    val dialog = new LoginDialog
    dialog.result
  }

}