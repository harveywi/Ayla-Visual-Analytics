/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.protocol

import ayla.client._
import ayla.server._
import java.io._
import ayla.client.ui.ConfirmationDialog
import ayla.geometry.ct.ContourTree
import ayla.geometry.ct.ContourTreeNode
import ayla.client.ui.DatasetExplorer
import scala.collection._
import ayla.collab.ConformationAnnotation

import shapeless._
import ayla.pickling2.Pickling._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2

case class CreateChatMessage(username: String, message: String) extends MsgFromClient{
  def pickled: String = CreateChatMessage.pickler.pickle(this)
  def serverDo(server: AylaServer, oosServer: ObjectOutputStream) = server.logChatMessage(username, message)
}

object CreateChatMessage {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[CreateChatMessage].create()
  PicklerRegistry2.register(picklerUnpickler[CreateChatMessage].create())
//  makeUnpickler(iso,  parse(_.toString) :: parse(_.toString) :: HNil)
}