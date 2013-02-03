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

case class GetColorFunctionRequest(username: String, file: File) extends MsgFromClient {
  def serverDo(server: AylaServer, oosServer: ObjectOutputStream) = replyWith(oosServer) {
    // This finds all matches in the unsampled dataset.
    val func = server.userSessions.find(_.username == username) match {
      case Some(session) =>
       session.dataset.getScalarArray(file)
      case None => throw new RuntimeException
    }
    
    GetColorFunctionResponse(func)
  }
}

case class GetColorFunctionResponse(f: Array[Float]) extends MsgFromServer {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
    client.EventStreams.colorFunction.fire(f)
  }
}
