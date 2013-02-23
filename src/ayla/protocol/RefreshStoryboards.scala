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

import shapeless._
import ayla.pickling2.Pickling._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2

case class RefreshStoryboardsRequest(username: String) extends MsgFromClient {
  def pickled(daos: java.io.DataOutputStream) = RefreshStoryboardsRequest.pickler.pickle(this, daos)
  def serverDo(server: AylaServer, daosServer: DataOutputStream) = replyWith(daosServer) {
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
  val (pickler, unpickler) = picklerUnpickler[RefreshStoryboardsRequest].create()
  PicklerRegistry2.register(picklerUnpickler[RefreshStoryboardsRequest].create())
}

case class RefreshStoryboardsResponse(storyboardsFromServer: Array[Storyboard]) extends MsgFromServer {
  def pickled(daos: java.io.DataOutputStream) = RefreshStoryboardsResponse.pickler.pickle(this, daos)
  def clientDo(client: AylaClient, daosClient: DataOutputStream) = {

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
  implicit def iso2 = Storyboard.iso
  implicit def iso3 = ConformationAnnotation.iso
  implicit val (p2, u2) = picklerUnpickler[ConformationAnnotation].create()
  implicit val (p, u) = picklerUnpickler[Storyboard].create()
  val (pickler, unpickler) = picklerUnpickler[RefreshStoryboardsResponse].create()
  PicklerRegistry2.register(picklerUnpickler[RefreshStoryboardsResponse].create())
}
