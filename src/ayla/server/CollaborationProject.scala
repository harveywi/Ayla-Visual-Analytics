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

import ayla.pickling._
import ayla.pickling.CanUnpickle._
import shapeless._
import shapeless.Functions._

@SerialVersionUID(1L)
case class CollaborationProject(projName: String, sf: ScalarFunction, sampledToUnsampled: Array[Int]) extends CanPickle[CollaborationProject]
object CollaborationProject extends CanUnpickle(parse(_.toString) :: ((s: String) => ScalarFunction.unpickle(s).get) :: ((s: String) => tokenize(s).map(_.toInt).toArray) :: HNil) {
  type CaseClass = CollaborationProject
  implicit def iso = Iso.hlist(CollaborationProject.apply _, CollaborationProject.unapply _)
  PickleRegistry.register(this.unpickle(_))
}