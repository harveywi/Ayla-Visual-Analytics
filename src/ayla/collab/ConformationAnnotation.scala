/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.collab

import ayla.pickling._
import ayla.pickling.CanUnpickle._
import shapeless._
import shapeless.Functions._

case class ConformationAnnotation(val name: String, val sampledConformationID: Int, val terrainCameraTransform: Array[Double], val pdbLines: Array[String]) extends AylaAnnotation with CanPickle[ConformationAnnotation]

object ConformationAnnotation extends CanUnpickle(parse(_.toString) :: parse(_.toInt) :: ((s: String) => tokenize(s).map(_.toDouble).toArray) :: ((s: String) => tokenize(s).toArray) :: HNil) {
    type CaseClass = ConformationAnnotation
    implicit def iso = Iso.hlist(ConformationAnnotation.apply _, ConformationAnnotation.unapply _)
    PickleRegistry.register(this.unpickle(_))
  }
