/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.protocol

import akka.actor.{ Actor, ActorRef }
import java.io.{ DataInputStream, BufferedInputStream }
import java.io.EOFException

case object SocketDead
case object ListenToSocket
class SocketReaderActor(owner: ActorRef, in: DataInputStream, inBuff: BufferedInputStream) extends Actor {
  self ! ListenToSocket
  def blockForSocketMessage(): String = try {
    inBuff.mark(Int.MaxValue)
    val msg = in.readUTF
    inBuff.reset()
    msg
  } catch {
    case _@ e: Exception => {
      println("Got an exception:  " + e)
      println("Shutting down this actor.")
      owner ! SocketDead
      context.stop(self)
      ""
    }
  }
  def waitOnSocket: Receive = {
    case ListenToSocket =>
      val msg = blockForSocketMessage()
      owner ! msg
  }
  def receive = waitOnSocket
}
