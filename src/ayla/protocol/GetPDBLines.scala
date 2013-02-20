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

import ayla.pickling._
import ayla.pickling.CanUnpickle._
import shapeless._
import shapeless.Functions._

case class GetPDBLinesRequest(username: String, iSampled: Int) extends MsgFromClient[GetPDBLinesResponse] with CanPickle[GetPDBLinesRequest] {
  def serverDo[H1 <: HList](server: AylaServer, oosServer: ObjectOutputStream)(implicit iso: Iso[GetPDBLinesResponse, H1],
      mapFolder: MapFolder[H1, String, CanPickle.toPickle.type]) = replyWith(oosServer) {
    val (proj, dataset) = server.userSessions.find(_.username == username).map { session => (session.collabProject, session.dataset) }.get
    val pdbLines = dataset.pdbStreamProvider.getPDBLines(proj.sampledToUnsampled(iSampled))
    
    GetPDBLinesResponse(pdbLines)
  }
}

object GetPDBLinesRequest {
  implicit def iso = Iso.hlist(apply _, unapply _)
  makeUnpickler(iso, parse(_.toString) :: parse(_.toInt) :: HNil)
}

case class GetPDBLinesResponse(pdbLines: Array[String]) extends MsgFromServer with CanPickle[GetPDBLinesResponse] {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
    client.EventStreams.pdbLines.fire(pdbLines)
  }
}

object GetPDBLinesResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
  makeUnpickler(iso, ((s: String) => tokenize(s).toArray) :: HNil)
}
