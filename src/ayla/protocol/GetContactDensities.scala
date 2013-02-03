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

case class GetContactDensitiesRequest(username: String, residues: Array[Int]) extends MsgFromClient {
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

case class GetContactDensitiesResponse(densities: Array[Int]) extends MsgFromServer {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
    client.EventStreams.contactDensities.fire(densities)
  }
}
