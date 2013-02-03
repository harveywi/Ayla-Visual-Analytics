/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui

import javax.swing.BorderFactory
import scala.swing.Component
import ayla.client.ui.state.UIState

trait LineBordered extends ColorSchemed { this: Component =>
  override def applyColorScheme(scheme: ColorScheme) = {
    border = BorderFactory.createLineBorder(scheme.lineColor)
    super.applyColorScheme(scheme)
  }
}
