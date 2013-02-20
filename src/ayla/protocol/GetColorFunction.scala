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

import ayla.pickling._
import ayla.pickling.CanUnpickle._
import shapeless._
import shapeless.Functions._

case class GetColorFunctionRequest(username: String, file: File) extends MsgFromClient[GetColorFunctionResponse] with CanPickle[GetColorFunctionRequest] {
  def serverDo[H1 <: HList](server: AylaServer, oosServer: ObjectOutputStream)(implicit iso: Iso[GetColorFunctionResponse, H1],
      mapFolder: MapFolder[H1, String, CanPickle.toPickle.type]) = replyWith(oosServer) {
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
  makeUnpickler(iso, parse(_.toString) :: ((s: String) => new File(s)) :: HNil)
}

case class GetColorFunctionResponse(f: Array[Float]) extends MsgFromServer with CanPickle[GetColorFunctionResponse] {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
    client.EventStreams.colorFunction.fire(f)
  }
}

object GetColorFunctionResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
  makeUnpickler(iso, ((s: String) => tokenize(s).map(_.toFloat).toArray) :: HNil)
}
