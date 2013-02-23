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

import shapeless._
import ayla.pickling2.Pickling._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2

case class GetPDBLinesRequest(username: String, iSampled: Int) extends MsgFromClient {
  def pickled(daos: java.io.DataOutputStream) = GetPDBLinesRequest.pickler.pickle(this, daos)
  def serverDo(server: AylaServer, daosServer: DataOutputStream) = replyWith(daosServer) {
    val (proj, dataset) = server.userSessions.find(_.username == username).map { session => (session.collabProject, session.dataset) }.get
    val pdbLines = dataset.pdbStreamProvider.getPDBLines(proj.sampledToUnsampled(iSampled))
    
    GetPDBLinesResponse(pdbLines)
  }
}

object GetPDBLinesRequest {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[GetPDBLinesRequest].create()
  PicklerRegistry2.register(picklerUnpickler[GetPDBLinesRequest].create())
}

case class GetPDBLinesResponse(pdbLines: Array[String]) extends MsgFromServer {
  def pickled(daos: java.io.DataOutputStream) = GetPDBLinesResponse.pickler.pickle(this, daos)
  def clientDo(client: AylaClient, daosClient: DataOutputStream) = {
    client.EventStreams.pdbLines.fire(pdbLines)
  }
}

object GetPDBLinesResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[GetPDBLinesResponse].create()
  PicklerRegistry2.register(picklerUnpickler[GetPDBLinesResponse].create())
}
