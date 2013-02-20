/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client

import java.io._
import java.net._
import akka.actor._
import ayla.client.ui.ConfirmationDialog
import ayla.client.ui.LoginInfo
import ayla.protocol._
import ayla.pickling.CanPickle

class ClientActor(aylaClient: AylaClient) extends Actor {

  var out: ObjectOutputStream = null
  var in: ObjectInputStream = null

  def receive = {
    case loginInfo: LoginInfo =>
      out = new ObjectOutputStream(new DataOutputStream(loginInfo.socket.getOutputStream()))
      
      in = new ObjectInputStream(loginInfo.socket.getInputStream())
      val socketReaderActor = context.system.actorOf(Props(new SocketReaderActor(this.self, in)))
      out.writeObject(ClientConnectRequest(loginInfo.username))
      out.flush

    case m: MsgFromServer =>
      m.clientDo(aylaClient, out)
      
    case m: MsgFromClient[_] =>
      println("ClientActor is sending message to server:  " + m)
      if (!m.isInstanceOf[Serializable]) {
        println(m.getClass)
        println("Not serializable!")
      }
      out.writeObject(m)
      out.flush
      
    case SocketDead =>
      val message = "The server connection has closed.  Would you like to try to reconnect?"
      ConfirmationDialog.getChoice("Server Connection Closed", message, "Reconnect", "Quit") match {
        case "Reconnect" =>
          aylaClient.connectToServer()
        case "Quit" => System.exit(0)
      }
      
    case _ => println("Got something weird")
  }
}
