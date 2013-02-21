/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.server

import ayla.dataset.CachedDataset
import ayla.geometry.ScalarFunction

import shapeless._
import ayla.pickling2.Pickling._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2

@SerialVersionUID(1L)
case class CollaborationProject(projName: String, sf: ScalarFunction, sampledToUnsampled: Array[Int])
object CollaborationProject {
  implicit def iso = Iso.hlist(apply _, unapply _)
  
  implicit def iso2 = ScalarFunction.iso
  implicit val (p, u) = picklerUnpickler[ScalarFunction].create()
  PicklerRegistry2.register(picklerUnpickler[CollaborationProject].create())
}
//object CollaborationProject extends CanUnpickle(parse(_.toString) :: ((s: String) => ScalarFunction.unpickle(s).get) :: ((s: String) => tokenize(s).map(_.toInt).toArray) :: HNil) {
//  type CaseClass = CollaborationProject
//  implicit def iso = Iso.hlist(CollaborationProject.apply _, CollaborationProject.unapply _)
//  PickleRegistry.register(this.unpickle(_))
//}