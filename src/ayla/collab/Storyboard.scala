/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.collab

import java.util.Date

@SerialVersionUID(1L)
class Storyboard(val name: String, val annotations: Array[ConformationAnnotation]) extends Serializable {
  val timestamp = new Date
  override def toString = "%s [%s]".format(name, timestamp.toString)
}
