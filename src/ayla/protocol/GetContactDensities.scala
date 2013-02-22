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

case class GetContactDensitiesRequest(username: String, residues: Array[Int]) extends MsgFromClient {
  def pickled: String = GetContactDensitiesRequest.pickler.pickle(this)
  def serverDo(server: AylaServer, oosServer: ObjectOutputStream) = replyWith(oosServer) {
    // This finds all matches in the unsampled dataset.
    val densities = server.userSessions.find(_.username == username) match {
      case Some(session) =>
        session.collabProject.sampledToUnsampled.par.map(i => session.dataset.pdbStreamProvider.getContactDensity(i, residues)).toArray
      case None => throw new RuntimeException
    }

    GetContactDensitiesResponse(densities)
  }
}

object GetContactDensitiesRequest {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[GetContactDensitiesRequest].create()
  PicklerRegistry2.register(picklerUnpickler[GetContactDensitiesRequest].create())
}

case class GetContactDensitiesResponse(densities: Array[Int]) extends MsgFromServer {
  def pickled: String = GetContactDensitiesResponse.pickler.pickle(this)
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
    client.EventStreams.contactDensities.fire(densities)
  }
}

object GetContactDensitiesResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[GetContactDensitiesResponse].create()
  PicklerRegistry2.register(picklerUnpickler[GetContactDensitiesResponse].create())
}
