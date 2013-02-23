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
import shapeless._

import ayla.pickling2.Pickling._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2
import ayla.pickling2.Picklable

case class ClientConnectRequest(username: String) extends MsgFromClient with Picklable {
	def pickled(daos: java.io.DataOutputStream) = ClientConnectRequest.pickler.pickle(this, daos)
	
  def serverDo(server: AylaServer, daosServer: DataOutputStream) = replyWith(daosServer) {
    println("server is trying to register username:  " + username)
    server.userSessions.find(_.username == username) match {
      case Some(_) => ClientConnectResponse(username, false, s"Someone already registered that user name ($username).")
      case None => ClientConnectResponse(username, true, "Welcome aboard, " + username)
    }
  }
}
object ClientConnectRequest {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[ClientConnectRequest].create()
  
  PicklerRegistry2.register(picklerUnpickler[ClientConnectRequest].create())
}

case class ClientConnectResponse(userName: String, connectionAccepted: Boolean, message: String) extends MsgFromServer {
  def pickled(daos: java.io.DataOutputStream) = ClientConnectResponse.pickler.pickle(this, daos)
  def clientDo(client: AylaClient, daosClient: DataOutputStream) = {
    if (connectionAccepted) {
      println("Connection accepted!")
      client.userName = userName
      replyWith(daosClient) {
        // Request the list of collaboration projects
        GetCollabProjectsRequest(userName)
      }
    } else {
      val dialogText = s"Couldn't connect to the server:  $message  Would you like to try to reconnect?"
      ConfirmationDialog.getChoice("Try Again?", dialogText, "Reconnect", "Quit") match {
        case "Reconnect" => client.connectToServer()
        case "Quit" => System.exit(0)
      }
    }
  }
}
object ClientConnectResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[ClientConnectResponse].create()
  PicklerRegistry2.register(picklerUnpickler[ClientConnectResponse].create())
}
