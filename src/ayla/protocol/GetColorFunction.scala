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
import java.io._
import scala.util.matching.Regex

import shapeless._
import ayla.pickling2.Pickling._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2

case class GetColorFunctionRequest(username: String, file: File) extends MsgFromClient {
  def pickled: String = GetColorFunctionRequest.pickler.pickle(this)
  def serverDo(server: AylaServer, oosServer: ObjectOutputStream) = replyWith(oosServer) {
    // This finds all matches in the unsampled dataset.
    val func = server.userSessions.find(_.username == username) match {
      case Some(session) =>
       session.dataset.getScalarArray(file)
      case None => throw new RuntimeException
    }
    
    GetColorFunctionResponse(func)
  }
}

object GetColorFunctionRequest {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[GetColorFunctionRequest].create()
  PicklerRegistry2.register(picklerUnpickler[GetColorFunctionRequest].create())
}

case class GetColorFunctionResponse(f: Array[Float]) extends MsgFromServer {
  def pickled: String = GetColorFunctionResponse.pickler.pickle(this)
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
    client.EventStreams.colorFunction.fire(f)
  }
}

object GetColorFunctionResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[GetColorFunctionResponse].create()
  PicklerRegistry2.register(picklerUnpickler[GetColorFunctionResponse].create())
}
