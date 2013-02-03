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

case class GetPDBLinesRequest(username: String, iSampled: Int) extends MsgFromClient {
  def serverDo(server: AylaServer, oosServer: ObjectOutputStream) = replyWith(oosServer) {
    val (proj, dataset) = server.userSessions.find(_.username == username).map { session => (session.collabProject, session.dataset) }.get
    val pdbLines = dataset.pdbStreamProvider.getPDBLines(proj.sampledToUnsampled(iSampled))
    
    GetPDBLinesResponse(pdbLines)
  }
}

case class GetPDBLinesResponse(pdbLines: Array[String]) extends MsgFromServer {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
    client.EventStreams.pdbLines.fire(pdbLines)
  }
}
