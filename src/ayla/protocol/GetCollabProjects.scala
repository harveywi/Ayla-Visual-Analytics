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
import ayla.client.ui.ConfirmationDialog
import ayla.client.ui.menu.PieMenu
import ayla.client.ui.menu.PieMenuItem
import ayla.client.ui.menu.PieMenuRoot
import ayla.client.ui._
import java.io._

import ayla.pickling._
import ayla.pickling.CanUnpickle._
import shapeless._
import shapeless.Functions._

case class GetCollabProjectsRequest(userName: String) extends MsgFromClient[GetCollabProjectsResponse] with CanPickle[GetCollabProjectsRequest] {
  def serverDo[H1 <: HList](server: AylaServer, oosServer: ObjectOutputStream)(implicit iso: Iso[GetCollabProjectsResponse, H1],
      mapFolder: MapFolder[H1, String, CanPickle.toPickle.type]) = replyWith(oosServer) {
    println("Server is preparing the list of collaboration projects")
    GetCollabProjectsResponse(server.datasets, server.scalarFunctions)
  }
}
object GetCollabProjectsRequest {
  implicit def iso = Iso.hlist(GetCollabProjectsRequest.apply _, GetCollabProjectsRequest.unapply _)
  makeUnpickler(iso, parse(_.toString) :: HNil)
}

case class GetCollabProjectsResponse(projHierarchy: Map[String, Array[String]], scalarFunctionMap: Map[String, Array[String]]) extends MsgFromServer with CanPickle[GetCollabProjectsResponse] {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
    println("Got the stuff!")
    projHierarchy.foreach(println)
    // Show the pie menu with the various projects 
    val g = PieMenu.newGraph()
    projHierarchy.keys.foreach { datasetName =>
      val item1 = new PieMenuItem(datasetName, None)
      g.addVertex(item1)
      g.addEdge(PieMenuRoot, item1)

      projHierarchy(datasetName).foreach { projName =>
        val item2 = new PieMenuItem(projName, None)
        g.addVertex(item2)
        g.addEdge(item1, item2)

        scalarFunctionMap(datasetName).foreach { sfName =>
          val item3 = new PieMenuItem(sfName, Some(OpenProjectDialogData(datasetName, projName, sfName)))
          g.addVertex(item3)
          g.addEdge(item2, item3)
        }
      }
    }

    val pieMenu = new PieMenu(g)
    OpenProjectDialog.chooseProject(pieMenu) match {
      case Some(projDescriptor) => {
        println("You chose a project descriptor:  " + projDescriptor)
        // Request the project from the server
        replyWith(oosClient) { ConnectToProjectRequest(projDescriptor.datasetName, projDescriptor.projName, projDescriptor.scalarFunctionName, client.userName) }
      }
      case None => System.exit(0)
    }
  }
}

object GetCollabProjectsResponse {
  implicit def iso = Iso.hlist(GetCollabProjectsResponse.apply _, GetCollabProjectsResponse.unapply _)

  val parseMap = (s: String) =>
    tokenize(s).map { t =>
      tokenize(t) match {
        case List(key, values) => key -> tokenize(values).toArray
      }
    }.toMap

    makeUnpickler(iso, parseMap :: parseMap :: HNil)
}
