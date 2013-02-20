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

import ayla.pickling._
import ayla.pickling.CanUnpickle._
import shapeless._
import shapeless.Functions._

case class CreateChatMessage(username: String, message: String) extends MsgFromClient[CreateChatMessageResponse] with CanPickle[CreateChatMessage] {
  def serverDo[H1 <: HList](server: AylaServer, oosServer: ObjectOutputStream)(implicit iso: Iso[CreateChatMessageResponse, H1],
      mapFolder: MapFolder[H1, String, CanPickle.toPickle.type]) = server.logChatMessage(username, message)
}

object CreateChatMessage {
  implicit def iso = Iso.hlist(CreateChatMessage.apply _, CreateChatMessage.unapply _)
  makeUnpickler(iso,  parse(_.toString) :: parse(_.toString) :: HNil)
}

case class CreateChatMessageResponse(msg: String) extends MsgFromServer {
  def clientDo(client: AylaClient, oosReply: ObjectOutputStream): Unit = {}
}
object CreateChatMessageResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
}