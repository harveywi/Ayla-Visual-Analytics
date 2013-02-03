/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui.menu

import scala.swing.Publisher
import scala.swing.event.Event

class RingMenuItem(val text: String) extends Publisher {
  
  def handleClick(progressListener: RingMenuProgressListener): Unit = {}
  
//  override def toString = text + "(" + this + ")"
  
}
