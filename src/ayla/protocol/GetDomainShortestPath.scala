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
import org.jgrapht.alg.DijkstraShortestPath
import scala.collection.JavaConverters._

import shapeless._
import ayla.pickling2.Pickling._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2

case class GetDomainShortestPathRequest(username: String, idStart: Int, idEnd: Int) extends MsgFromClient{
  def serverDo(server: AylaServer, oosServer: ObjectOutputStream) = replyWith(oosServer) {
    val domainGraph = server.userSessions.find(_.username == username).map { session => session.domainGraph }.get

    val dsp = new DijkstraShortestPath(domainGraph, idStart, idEnd)
    val edgeList = dsp.getPathEdgeList()
    val pathVerts = new scala.collection.mutable.ArrayBuffer[Int]
    pathVerts += idStart
    edgeList.asScala.foreach { e =>
      val v1 = domainGraph.getEdgeSource(e)
      val v2 = domainGraph.getEdgeTarget(e)
      if (pathVerts.last == v1) {
        pathVerts += v2
      } else {
        pathVerts += v1
      }
    }

    GetDomainShortestPathResponse(pathVerts.toArray)
  }
}

object GetDomainShortestPathRequest {
  implicit def iso = Iso.hlist(apply _, unapply _)
  PicklerRegistry2.register(picklerUnpickler[GetDomainShortestPathRequest].create())
}

case class GetDomainShortestPathResponse(pathVerts: Array[Int]) extends MsgFromServer {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
  	client.EventStreams.domainShortestPath.fire(pathVerts)
  }
}
object GetDomainShortestPathResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
  PicklerRegistry2.register(picklerUnpickler[GetDomainShortestPathResponse].create())
}
