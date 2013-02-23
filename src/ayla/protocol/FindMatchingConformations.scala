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

case class FindMatchingConformationsRequest(username: String, regex: Regex) extends MsgFromClient {
  def pickled(daos: java.io.DataOutputStream) = FindMatchingConformationsRequest.pickler.pickle(this, daos)
  def serverDo(server: AylaServer, daosServer: DataOutputStream) = replyWith(daosServer) {
    // This finds all matches in the unsampled dataset.
    val matchResults = server.userSessions.find(_.username == username) match {
      case Some(session) =>
        val resultUnsampled = session.dataset.pdbStreamProvider.findMatchingConformations(regex).map(_.swap).toMap
        session.collabProject.sampledToUnsampled.zipWithIndex.flatMap {
          case (u, s) =>
            resultUnsampled.get(u) match {
              case Some(str) => { Some(str, s) }
              case None => None
            }
        }.toArray

      case None => throw new RuntimeException
    }

    FindMatchingConformationsResponse(matchResults)
  }
}

object FindMatchingConformationsRequest {
  implicit def iso = Iso.hlist(FindMatchingConformationsRequest.apply _, FindMatchingConformationsRequest.unapply _)
  val (pickler, unpickler) = picklerUnpickler[FindMatchingConformationsRequest].create()
  PicklerRegistry2.register(picklerUnpickler[FindMatchingConformationsRequest].create())
}

case class FindMatchingConformationsResponse(matchResults: Array[(String, Int)]) extends MsgFromServer {
  def pickled(daos: java.io.DataOutputStream) = FindMatchingConformationsResponse.pickler.pickle(this, daos)
  def clientDo(client: AylaClient, daosClient: DataOutputStream) = {
    client.EventStreams.matchingConformations.fire(matchResults)
  }
}

object FindMatchingConformationsResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[FindMatchingConformationsResponse].create()
  PicklerRegistry2.register(picklerUnpickler[FindMatchingConformationsResponse].create())
}
