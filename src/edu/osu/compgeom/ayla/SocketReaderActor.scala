package edu.osu.compgeom.ayla

import akka.actor.{Actor, ActorRef}
import java.io.ObjectInputStream
import java.io.EOFException

class SocketReaderActor(owner: ActorRef, in: ObjectInputStream) extends Actor {
  case object ListenToSocket
  self ! ListenToSocket
  def blockForSocketMessage(): Any = try { in.readObject } catch { case _ @ e => {
    println("Got an exception:  " + e)
    println("Shutting down this actor.")
    context.stop(self)
  } }
  def waitOnSocket: Receive = {
    case ListenToSocket => {
      val msg = blockForSocketMessage()
      self ! ListenToSocket
      owner ! msg
    }
  }
  def receive = waitOnSocket
}