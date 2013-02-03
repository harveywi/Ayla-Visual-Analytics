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
import ayla.client.ui.event.ColorSchemeChanged
import ayla.client.ui.state.UIState

/**
 * A UIElement which has the fancy Ayla color scheme, and responds to changes in the color theme
 * @author harveywi
 *
 */
trait ColorSchemed {this: UIElement =>
  applyColorScheme(UIState.colorScheme)
  listenTo(UIState)
  
  def applyColorScheme(scheme: ColorScheme) = {
  	this match {
  	  case _: Button | _: TextField | _: Label | _: TextArea => {
  	    background = scheme.btnBackground
  	    foreground = scheme.btnForeground
  	  }
  	  case _ => background = scheme.bgColor
  	}
  	repaint()
  }
  
	reactions += {
	  case ColorSchemeChanged(newScheme) => applyColorScheme(newScheme)
	}
}
