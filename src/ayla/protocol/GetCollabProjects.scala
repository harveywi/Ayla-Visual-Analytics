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

import shapeless._
import ayla.pickling2.Pickling._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2

case class GetCollabProjectsRequest(userName: String) extends MsgFromClient {
  def pickled(daos: java.io.DataOutputStream) = GetCollabProjectsRequest.pickler.pickle(this, daos)
  def serverDo(server: AylaServer, daosServer: DataOutputStream) = replyWith(daosServer) {
    println("Server is preparing the list of collaboration projects")
    GetCollabProjectsResponse(server.datasets, server.scalarFunctions)
  }
}
object GetCollabProjectsRequest {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[GetCollabProjectsRequest].create()
  PicklerRegistry2.register(picklerUnpickler[GetCollabProjectsRequest].create())
}

case class GetCollabProjectsResponse(projHierarchy: Map[String, Array[String]], scalarFunctionMap: Map[String, Array[String]]) extends MsgFromServer {
  def pickled(daos: java.io.DataOutputStream) = GetCollabProjectsResponse.pickler.pickle(this, daos)
  def clientDo(client: AylaClient, daosClient: DataOutputStream) = {
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
        replyWith(daosClient) { ConnectToProjectRequest(projDescriptor.datasetName, projDescriptor.projName, projDescriptor.scalarFunctionName, client.userName) }
      }
      case None => System.exit(0)
    }
  }
}

object GetCollabProjectsResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[GetCollabProjectsResponse].create()
  PicklerRegistry2.register(picklerUnpickler[GetCollabProjectsResponse].create())
}
