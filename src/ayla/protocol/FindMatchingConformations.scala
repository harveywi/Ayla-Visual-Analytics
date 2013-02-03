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

case class FindMatchingConformationsRequest(username: String, regex: Regex) extends MsgFromClient {
  def serverDo(server: AylaServer, oosServer: ObjectOutputStream) = replyWith(oosServer) {
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

case class FindMatchingConformationsResponse(matchResults: Array[(String, Int)]) extends MsgFromServer {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
    client.EventStreams.matchingConformations.fire(matchResults)
  }
}
