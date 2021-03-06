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
import ayla.pickling2.Picklable

sealed trait AylaMsg extends Picklable {
  def replyWith(daos: DataOutputStream)(op: => AylaMsg) = {
    val x = op
    x.pickled(daos)
    daos.flush()
  }
}

trait MsgFromClient extends AylaMsg {
  def serverDo(server: AylaServer, daosReply: DataOutputStream)
}

trait MsgFromServer extends AylaMsg {
  def clientDo(client: AylaClient, daosReply: DataOutputStream)
}

trait MsgFromServerReactive[T <: Serializable] extends MsgFromServer with Picklable {
  def data: T
  def es(client: AylaClient): EventSource[T]
  
  override def clientDo(client: AylaClient, daosReply: DataOutputStream): Unit = {
    println("MsgFromServerReactive serverDo was called")
    es(client).fire(data)
  }
}

object MsgFromServerReactive {
  def apply[T <: Serializable](dataToSend: T, eventSource: AylaClient => EventSource[T]) = ???
//  def apply[T <: Serializable](dataToSend: T, eventSource: AylaClient => EventSource[T]) = new MsgFromServerReactive[T] {
//    val data = dataToSend
//    def es(client: AylaClient) = eventSource(client)
//  }
}
