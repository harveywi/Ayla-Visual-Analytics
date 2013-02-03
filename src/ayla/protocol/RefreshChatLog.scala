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

case class RefreshChatLogRequest(username: String) extends MsgFromClient {
  def serverDo(server: AylaServer, oosServer: ObjectOutputStream) = replyWith(oosServer) {
    server.userSessions.find(_.username == username) match {
      case Some(session) =>
        val chatLog = server.chatMap.getOrElseUpdate(session.projInfo, new mutable.ArrayBuffer[String]).toArray
        RefreshChatLogResponse(chatLog)
      case None => throw new RuntimeException
    }
  }
}

case class RefreshChatLogResponse(chatLog: Array[String]) extends MsgFromServer {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
  	client.collabFrame.chatTextArea.text = chatLog.mkString("\n")
  }
}
