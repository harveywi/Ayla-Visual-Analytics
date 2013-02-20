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

import ayla.pickling._
import ayla.pickling.CanUnpickle._
import shapeless._
import shapeless.Functions._

case class ClientConnectRequest(username: String) extends MsgFromClient[ClientConnectResponse] with CanPickle[ClientConnectRequest] {
  def serverDo[H1 <: HList](server: AylaServer, oosServer: ObjectOutputStream)(
      implicit iso: Iso[ClientConnectResponse, H1],
      mapFolder: MapFolder[H1, String, CanPickle.toPickle.type]) = replyWith(oosServer) {
    println("server is trying to register username:  " + username)
    server.userSessions.find(_.username == username) match {
      case Some(_) => ClientConnectResponse(username, false, s"Someone already registered that user name ($username).")
      case None => ClientConnectResponse(username, true, "Welcome aboard, " + username)
    }
  }
}
object ClientConnectRequest {
  implicit def iso = Iso.hlist(apply _, unapply _)
  makeUnpickler(iso, parse(_.toString) :: HNil)
}

case class ClientConnectResponse(userName: String, connectionAccepted: Boolean, message: String) extends MsgFromServer with CanPickle[ClientConnectResponse] {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
    if (connectionAccepted) {
      println("Connection accepted!")
      client.userName = userName
      replyWith(oosClient) {
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
  implicit def iso = Iso.hlist(ClientConnectResponse.apply _, ClientConnectResponse.unapply _)
  makeUnpickler(iso, parse(_.toString) :: parse(_.toBoolean) :: parse(_.toString) :: HNil)
}
