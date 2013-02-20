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
import ayla.collab.Storyboard
import ayla.client.ui.event.StoryboardsRefreshed
import scala.collection._

import ayla.pickling._
import ayla.pickling.CanUnpickle._
import shapeless._
import shapeless.Functions._

case class RefreshStoryboardsRequest(username: String) extends MsgFromClient[RefreshStoryboardsResponse] with CanPickle[RefreshStoryboardsRequest] {
  def serverDo[H1 <: HList](server: AylaServer, oosServer: ObjectOutputStream)(implicit iso: Iso[RefreshStoryboardsResponse, H1],
      mapFolder: MapFolder[H1, String, CanPickle.toPickle.type]) = replyWith(oosServer) {
    server.userSessions.find(_.username == username) match {
      case Some(session) =>
        val storyboards = server.storyboardMap.getOrElseUpdate(session.projInfo, new mutable.ArrayBuffer[Storyboard]).toArray
        RefreshStoryboardsResponse(storyboards)
      case None => throw new RuntimeException
    }
  }
}

object RefreshStoryboardsRequest {
  implicit def iso = Iso.hlist(apply _, unapply _)
  makeUnpickler(iso, parse(_.toString) :: HNil)
}

case class RefreshStoryboardsResponse(storyboardsFromServer: Array[Storyboard]) extends MsgFromServer with CanPickle[RefreshStoryboardsResponse] {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {

    val curAnnotations = client.collabFrame.annotationListView.listData.map(_.annotation)
    val storyboards = storyboardsFromServer.map { storyboard =>
      {
        val localAnnotations = storyboard.annotations.map { serverAnn =>
          curAnnotations.find(a => a.name == serverAnn.name && a.timestamp == serverAnn.timestamp && a.sampledConformationID == serverAnn.sampledConformationID) match {
            case Some(ann) => ann
            case None => { throw new RuntimeException("Error:  Client doesn't have an annotation that was found in a server storyboard.") }
          }
        }
        new Storyboard(storyboard.name, localAnnotations)
      }
    }

    // TODO there is a problem here
    // The annotation objects in the storyboards that we get from the server need to be
    // wired up to local cached copies of the annotations
    client.collabFrame.storyboardPanel.savedStoryboardsListView.listData = storyboards
    client.publish(StoryboardsRefreshed(storyboards))
  }
}

object RefreshStoryboardsResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
  makeUnpickler(iso, ((s: String) => tokenize(s).map(t => Storyboard.unpickle(t).get).toArray) :: HNil)
}
