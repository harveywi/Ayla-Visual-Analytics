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

import shapeless._
import ayla.pickling2.Pickling._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2

case class RefreshChatLogRequest(username: String) extends MsgFromClient {
  def pickled: String = RefreshChatLogRequest.pickler.pickle(this)
  def serverDo(server: AylaServer, oosServer: ObjectOutputStream) = replyWith(oosServer) {
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
  val (pickler, unpickler) = picklerUnpickler[RefreshChatLogRequest].create()
  PicklerRegistry2.register(picklerUnpickler[RefreshChatLogRequest].create())
}

case class RefreshChatLogResponse(chatLog: Array[String]) extends MsgFromServer {
  def pickled = RefreshChatLogResponse.pickler.pickle(this)
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
  	client.collabFrame.chatTextArea.text = chatLog.mkString("\n")
  }
}

object RefreshChatLogResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[RefreshChatLogResponse].create()
  PicklerRegistry2.register(picklerUnpickler[RefreshChatLogResponse].create())
}
