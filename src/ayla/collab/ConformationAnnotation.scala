/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.collab

import shapeless._
import ayla.pickling2.Pickling._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2
import ayla.pickling2.Picklable

case class ConformationAnnotation(val name: String, val sampledConformationID: Int, val terrainCameraTransform: Array[Double], val pdbLines: Array[String]) extends AylaAnnotation with Picklable {
  def pickled: String = ConformationAnnotation.pickler.pickle(this)
}

object ConformationAnnotation {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[ConformationAnnotation].create()
  PicklerRegistry2.register((pickler, unpickler))
}

//object ConformationAnnotation extends CanUnpickle(parse(_.toString) :: parse(_.toInt) :: ((s: String) => tokenize(s).map(_.toDouble).toArray) :: ((s: String) => tokenize(s).toArray) :: HNil) {
//    type CaseClass = ConformationAnnotation
//    implicit def iso = Iso.hlist(ConformationAnnotation.apply _, ConformationAnnotation.unapply _)
//    PickleRegistry.register(this.unpickle(_))
//  }
