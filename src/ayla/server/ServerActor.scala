/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.server

import java.net.InetSocketAddress
import akka.actor._
import akka.util.ByteString
import ayla.protocol._
import java.io._
import scala.collection.mutable.ArrayBuffer
import java.net._
import ayla.pickling2.PicklerRegistry2

class ServerActor(aylaServer: AylaServer, socket: Socket) extends Actor {
  val out = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))
  val inBuff = new BufferedInputStream(socket.getInputStream)
  val in = new DataInputStream(inBuff)
  val socketReaderActor = context.system.actorOf(Props(new SocketReaderActor(this.self, in, inBuff)))

  // TODO handle SocketDead message
  def receive = {
    case className: String if !className.isEmpty=>
      PicklerRegistry2.unpickle(className, in) match {
        case m: ClientConnectRequest =>
          socketReaderActor ! ListenToSocket
          println(s"Associating user name ${m.username} with an actor.")
          aylaServer.usernameToServerActor(m.username) = self
          m.serverDo(aylaServer, out)
        case m: MsgFromClient =>
          socketReaderActor ! ListenToSocket
          println("Server received message from client:  " + m)
          m.serverDo(aylaServer, out)
        case x @ _ => println("Server received unexpected message:  " + className)
      }
    case m: MsgFromClient => 
      println("Server received message from client:  " + m)
      m.serverDo(aylaServer, out)
    case x @ _ => println("Server received weird message:  " + x)
  }
}
