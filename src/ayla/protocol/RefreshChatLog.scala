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
import ayla.collab.ConformationAnnotationListItem
import ayla.client.ui.event.AnnotationsRefreshed
import ayla.collab.ConformationAnnotation
import scala.collection._

import ayla.pickling._
import ayla.pickling.CanUnpickle._
import shapeless._
import shapeless.Functions._

case class RefreshChatLogRequest(username: String) extends MsgFromClient[RefreshChatLogResponse] with CanPickle[RefreshChatLogRequest] {
  def serverDo[H1 <: HList](server: AylaServer, oosServer: ObjectOutputStream)(implicit iso: Iso[RefreshChatLogResponse, H1],
      mapFolder: MapFolder[H1, String, CanPickle.toPickle.type]) = replyWith(oosServer) {
    server.userSessions.find(_.username == username) match {
      case Some(session) =>
        val chatLog = server.chatMap.getOrElseUpdate(session.projInfo, new mutable.ArrayBuffer[String]).toArray
        RefreshChatLogResponse(chatLog)
      case None => throw new RuntimeException
    }
  }
}

object RefreshChatLogRequest {
  implicit def iso = Iso.hlist(apply _, unapply _)
  makeUnpickler(iso, parse(_.toString) :: HNil)
}

case class RefreshChatLogResponse(chatLog: Array[String]) extends MsgFromServer with CanPickle[RefreshChatLogResponse] {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
  	client.collabFrame.chatTextArea.text = chatLog.mkString("\n")
  }
}

object RefreshChatLogResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
  makeUnpickler(iso, ((s: String) => tokenize(s).toArray) :: HNil)
}
