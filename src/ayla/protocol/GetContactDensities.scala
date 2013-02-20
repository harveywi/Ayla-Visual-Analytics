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

case class GetContactDensitiesRequest(username: String, residues: Array[Int]) extends MsgFromClient[GetContactDensitiesResponse] with CanPickle[GetContactDensitiesRequest] {
  def serverDo[H1 <: HList](server: AylaServer, oosServer: ObjectOutputStream)(implicit iso: Iso[GetContactDensitiesResponse, H1],
      mapFolder: MapFolder[H1, String, CanPickle.toPickle.type]) = replyWith(oosServer) {
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
  makeUnpickler(iso, parse(_.toString) :: ((s: String) => tokenize(s).map(_.toInt).toArray) :: HNil)
}

case class GetContactDensitiesResponse(densities: Array[Int]) extends MsgFromServer with CanPickle[GetContactDensitiesResponse] {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
    client.EventStreams.contactDensities.fire(densities)
  }
}

object GetContactDensitiesResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
  makeUnpickler(iso, ((s: String) => tokenize(s).map(_.toInt).toArray) :: HNil)
}
