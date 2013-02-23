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

import shapeless._
import ayla.pickling2.Pickling._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2

@SerialVersionUID(1L)
case class Storyboard(val name: String, val annotations: Array[ConformationAnnotation]) {
  val timestamp = new Date
  override def toString = "%s [%s]".format(name, timestamp.toString)
}

object Storyboard {
  implicit def iso = Iso.hlist(Storyboard.apply _, Storyboard.unapply _)
  implicit val (p, u) = picklerUnpickler[ConformationAnnotation].create()
  val (pickler, unpickler) = picklerUnpickler[Storyboard].create()
}
