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

class ServerActor(aylaServer: AylaServer, socket: Socket) extends Actor {
  val out = new ObjectOutputStream(socket.getOutputStream())
  val in = new ObjectInputStream(new DataInputStream(socket.getInputStream()))
  val socketReaderActor = context.system.actorOf(Props(new SocketReaderActor(this.self, in)))

  def receive = {
    case m: ClientConnectRequest =>
      println(s"Associating user name ${m.username} with an actor.")
      aylaServer.usernameToServerActor(m.username) = self
      m.serverDo(aylaServer, out)
    case m: MsgFromClient => 
      println("Server received message from client:  " + m)
      m.serverDo(aylaServer, out)
    case x @ _ => println("Server received weird message:  " + x)
  }
}
