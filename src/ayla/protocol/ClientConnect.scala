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

case class ClientConnectRequest(username: String) extends MsgFromClient {
  def serverDo(server: AylaServer, oosServer: ObjectOutputStream) = replyWith(oosServer) {
    println("server is trying to register username:  " + username)
    server.userSessions.find(_.username == username) match {
      case Some(_) => ClientConnectResponse(username, false, s"Someone already registered that user name ($username).")
      case None => ClientConnectResponse(username, true, "Welcome aboard, " + username)
    }
  }
}

case class ClientConnectResponse(userName: String, connectionAccepted: Boolean, message: String) extends MsgFromServer {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
    if (connectionAccepted) {
      println("Connection accepted!")
      client.userName = userName
      replyWith(oosClient) {
        // Request the list of collaboration projects
        GetCollabProjectsRequest
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
