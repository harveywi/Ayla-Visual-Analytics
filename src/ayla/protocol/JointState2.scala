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
import scala.reflect._
import akka.actor._
import akka.actor.Actor
import java.io._
import reactive._

import ayla.pickling._
import ayla.pickling.CanPickle._
import shapeless._

sealed trait AylaMsg {
  def replyWith[P, H1 <: HList](oos: ObjectOutputStream)(op: => AylaMsg with CanPickle[P])(implicit iso: Iso[P, H1], mapFolder: MapFolder[H1, String, toPickle.type]) = {
    val x = op
    val pickle = x.pickle
    oos.writeObject(pickle)
    oos.flush()
//    val x = op
//    oos.writeObject(x)
//    oos.flush()
  }
}

trait MsgFromClient[R <: MsgFromServer] extends AylaMsg {
  def serverDo[H1 <: HList](server: AylaServer, oosReply: ObjectOutputStream)(implicit iso: Iso[R, H1],
      mapFolder: MapFolder[H1, String, CanPickle.toPickle.type])
}

trait MsgFromServer extends AylaMsg {
  def clientDo(client: AylaClient, oosReply: ObjectOutputStream)
  
}

trait MsgFromServerReactive[T <: Serializable] extends MsgFromServer with Serializable {
  def data: T
  def es(client: AylaClient): EventSource[T]
  
  override def clientDo(client: AylaClient, oosReply: ObjectOutputStream): Unit = {
    println("MsgFromServerReactive serverDo was called")
    es(client).fire(data)
  }
}

object MsgFromServerReactive {
  def apply[T <: Serializable](dataToSend: T, eventSource: AylaClient => EventSource[T]) = new MsgFromServerReactive[T] {
    val data = dataToSend
    def es(client: AylaClient) = eventSource(client)
  }
}
