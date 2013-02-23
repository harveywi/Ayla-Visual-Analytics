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

  var out: DataOutputStream = null
  
  var inBuff: BufferedInputStream = null
  var in: DataInputStream = null
  var socketReaderActor: ActorRef = null

  def receive = {
    case loginInfo: LoginInfo =>
      out = new DataOutputStream(loginInfo.socket.getOutputStream)
      
      inBuff = new BufferedInputStream(loginInfo.socket.getInputStream)
      in = new DataInputStream(inBuff)
      socketReaderActor = context.system.actorOf(Props(new SocketReaderActor(this.self, in, inBuff)))

      ClientConnectRequest(loginInfo.username).pickled(out)
      out.flush

//    case m: MsgFromServer =>
//      m.clientDo(aylaClient, out)
    
    case className: String =>
      PicklerRegistry2.unpickle(className, in) match {
        case m: MsgFromServer =>
          m.clientDo(aylaClient, out)
        case _ =>
          throw new RuntimeException("Unexpected unpickling occurred for class with name:  " + className)
      }
      socketReaderActor ! ListenToSocket
      
      
    case m: MsgFromClient =>
      println("ClientActor is sending message to server:  " + m)
      if (!m.isInstanceOf[Picklable]) {
        println(m.getClass)
        println("Not picklable:  " + m.getClass)
      }
      m.pickled(out)
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
