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

import ayla.pickling._
import ayla.pickling.CanUnpickle._
import shapeless._
import shapeless.Functions._

@SerialVersionUID(1L)
case class Storyboard(val name: String, val annotations: Array[ConformationAnnotation]) extends CanPickle[Storyboard] {
  val timestamp = new Date
  override def toString = "%s [%s]".format(name, timestamp.toString)
}

object Storyboard extends CanUnpickle(parse(_.toString) :: ((s: String) => tokenize(s).map(t => ConformationAnnotation.unpickle(t).get).toArray) :: HNil) {
    type CaseClass = Storyboard
    implicit def iso = Iso.hlist(Storyboard.apply _, Storyboard.unapply _)
    PickleRegistry.register(this.unpickle(_))
  }
