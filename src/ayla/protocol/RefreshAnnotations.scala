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

case class RefreshAnnotationsRequest(username: String) extends MsgFromClient {
  def serverDo(server: AylaServer, oosServer: ObjectOutputStream) = replyWith(oosServer) {
    server.userSessions.find(_.username == username) match {
      case Some(session) =>
        val annotations = server.annotationMap.getOrElseUpdate(session.projInfo, new mutable.ArrayBuffer[ConformationAnnotation]).toArray
        RefreshAnnotationsResponse(annotations)
      case None => throw new RuntimeException
    }
  }
}

case class RefreshAnnotationsResponse(annotations: Array[ConformationAnnotation]) extends MsgFromServer {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {

    val curAnnotationSet = client.collabFrame.annotationListView.listData.map(listItem => (listItem.annotation, listItem)).toMap
    val newListItems = annotations.map{a =>
      curAnnotationSet.get(a) match {
        case Some(listItem) => listItem
        case None => new ConformationAnnotationListItem(a)
      }
    }

    client.collabFrame.annotationListView.listData = newListItems
    client.publish(AnnotationsRefreshed(annotations))
  }
}
