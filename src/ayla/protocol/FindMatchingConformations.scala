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

case class FindMatchingConformationsRequest(username: String, regex: Regex) extends MsgFromClient[FindMatchingConformationsResponse] with CanPickle[FindMatchingConformationsRequest] {
  def serverDo[H1 <: HList](server: AylaServer, oosServer: ObjectOutputStream)(implicit iso: Iso[FindMatchingConformationsResponse, H1],
      mapFolder: MapFolder[H1, String, CanPickle.toPickle.type]) = replyWith(oosServer) {
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
  makeUnpickler(iso, parse(_.toString) :: ((s: String) => new Regex(s)) :: HNil)
}

case class FindMatchingConformationsResponse(matchResults: Array[(String, Int)]) extends MsgFromServer with CanPickle[FindMatchingConformationsResponse] {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
    client.EventStreams.matchingConformations.fire(matchResults)
  }
}

object FindMatchingConformationsResponse {
  implicit def iso = Iso.hlist(FindMatchingConformationsResponse.apply _, FindMatchingConformationsResponse.unapply _)
  makeUnpickler(iso, ((s: String) => tokenize(s).map(t => tokenize(t) match {
    case List(s, i) => (s, i.toInt)
  }).toArray) :: HNil)
}
