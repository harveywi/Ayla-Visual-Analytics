/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui.state

import ayla.client.ui.ColorScheme
import scala.swing.Publisher
import ayla.client.ui.event.ColorSchemeChanged

object UIState extends Publisher {
  
  private[this] var _colorScheme = ColorScheme.dark
  def colorScheme = _colorScheme
  def colorScheme_= (scheme: ColorScheme) = {
    _colorScheme = scheme
    publish(ColorSchemeChanged(_colorScheme))
  }
  
}
