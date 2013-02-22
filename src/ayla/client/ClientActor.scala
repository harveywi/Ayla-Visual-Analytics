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
import ayla.pickling2.PicklerRegistry2
import ayla.pickling2.Picklable

class ClientActor(aylaClient: AylaClient) extends Actor {

  var out: ObjectOutputStream = null
  var in: ObjectInputStream = null

  def receive = {
    case loginInfo: LoginInfo =>
      out = new ObjectOutputStream(new DataOutputStream(loginInfo.socket.getOutputStream()))
      
      in = new ObjectInputStream(loginInfo.socket.getInputStream())
      val socketReaderActor = context.system.actorOf(Props(new SocketReaderActor(this.self, in)))
      out.writeObject(ClientConnectRequest(loginInfo.username).pickled)
      out.flush

//    case m: MsgFromServer =>
//      m.clientDo(aylaClient, out)
    
    case s: String =>
      PicklerRegistry2.unpickle(s) match {
        case Some(m: MsgFromServer) =>
          m.clientDo(aylaClient, out)
        case _ =>
          throw new RuntimeException("Unexpected string message received:  " + s)
      }
      
    case m: MsgFromClient =>
      println("ClientActor is sending message to server:  " + m)
      if (!m.isInstanceOf[Picklable]) {
        println(m.getClass)
        println("Not picklable:  " + m.getClass)
      }
      out.writeObject(m.pickled)
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
