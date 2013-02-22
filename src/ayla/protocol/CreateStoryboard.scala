/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.protocol

import ayla.collab.Storyboard
import java.io._
import ayla.server.AylaServer
import ayla.client.AylaClient
import shapeless._
import ayla.pickling2.Pickling._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2
import ayla.collab.ConformationAnnotation

case class CreateStoryboard(username: String, storyboard: Storyboard) extends MsgFromClient {
  def pickled: String = CreateStoryboard.pickler.pickle(this)
  def serverDo(server: AylaServer, oosServer: ObjectOutputStream) = server.logStoryboard(username, storyboard)
}

object CreateStoryboard {
  implicit def iso = Iso.hlist(apply _, unapply _)
  implicit def iso2 = Storyboard.iso
  implicit def iso3 = ConformationAnnotation.iso
  implicit val (p2, u2) = picklerUnpickler[ConformationAnnotation].create()
  implicit val (p, u) = picklerUnpickler[Storyboard].create()
  val (pickler, unpickler) = picklerUnpickler[CreateStoryboard].create()
  PicklerRegistry2.register(picklerUnpickler[CreateStoryboard].create())
}