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
import java.io.ObjectInputStream
import java.io.EOFException

case object SocketDead
class SocketReaderActor(owner: ActorRef, in: ObjectInputStream) extends Actor {
  case object ListenToSocket
  self ! ListenToSocket
  def blockForSocketMessage(): Any = try { in.readObject } catch {
    case _@ e: Exception => {
      println("Got an exception:  " + e)
      println("Shutting down this actor.")
      owner ! SocketDead 
      context.stop(self)
    }
  }
  def waitOnSocket: Receive = {
    case ListenToSocket =>
      val msg = blockForSocketMessage()
      self ! ListenToSocket
      owner ! msg
  }
  def receive = waitOnSocket
}
