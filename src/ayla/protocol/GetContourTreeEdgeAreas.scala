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
import ayla.client.ui.ConfirmationDialog
import ayla.geometry.ct.ContourTree
import ayla.geometry.ct.ContourTreeNode
import ayla.client.ui.DatasetExplorer

import ayla.pickling._
import ayla.pickling.CanUnpickle._
import shapeless._
import shapeless.Functions._

case class GetContourTreeAreasRequest(username: String, vertBatches: Array[Array[Int]]) extends MsgFromClient[GetContourTreeAreasResponse] with CanPickle[GetContourTreeAreasRequest] {
  def serverDo[H1 <: HList](server: AylaServer, oosServer: ObjectOutputStream)(implicit iso: Iso[GetContourTreeAreasResponse, H1],
      mapFolder: MapFolder[H1, String, CanPickle.toPickle.type]) = replyWith(oosServer) {
    println("Contour tree edge area request received.")

    println("Server is estimating areas.")

    val (proj, dataset) = server.userSessions.find(_.username == username).map { session => (session.collabProject, session.dataset) }.get

    val areas = vertBatches.map { vertBatchSampled =>
      val vertIndices = vertBatchSampled.map(proj.sampledToUnsampled)
      server.estimateArea(vertIndices, dataset)
    }
    
    GetContourTreeAreasResponse(areas, dataset.dsspOutput, dataset.scalarArrayFiles)
  }
}
object GetContourTreeAreasRequest {
  implicit def iso = Iso.hlist(apply _, unapply _)
  makeUnpickler(iso, parse(_.toString) :: ((s: String) => tokenize(s).map(t => tokenize(t).map(_.toInt).toArray).toArray) :: HNil)
}

case class GetContourTreeAreasResponse(areas: Array[Double], dsspOutput: Option[Array[Char]], scalarArrays: Array[File]) extends MsgFromServer with CanPickle[GetContourTreeAreasResponse] {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = {
    val edges = client.ct.criticalNodeToIncidentEdges.values.flatten.toArray.distinct
    edges.iterator.zip(areas.iterator).foreach { case (e, area) => e.area = area }

    // Do some contour tree sanity checks
    import org.jgrapht.graph._
    import org.jgrapht.alg._
    val g = new SimpleGraph[ContourTreeNode, DefaultEdge](classOf[DefaultEdge])
    val nodesAugmented = client.ct.nodesAugmented
    nodesAugmented.foreach(v => g.addVertex(v))

    nodesAugmented.foreach { n =>
      n.parents.foreach(p => g.addEdge(n, p))
    }
    val ci = new ConnectivityInspector(g)

    println("Connected?  " + ci.isGraphConnected())
    println("num cc:  " + ci.connectedSets.size)
    println("Connected?  " + ci.isGraphConnected())
    println("Betti zero:  " + client.ct.scalarFunction.bettiZero)
    
    val nodeAtInfinity = client.ct.nodesContracted.maxBy(n => client.sf.getFuncVal(n.vertex))
    println("Node at infinity: " + nodeAtInfinity)
    
    val dataset = new AylaClientCachedDataset(client, dsspOutput, scalarArrays)
    println("Dataset:  " + dataset)
//    
    DatasetExplorer(client.ct, nodeAtInfinity, dataset)
    
    client.clientActor ! RefreshChatLogRequest(client.userName)
    client.clientActor ! RefreshAnnotationsRequest(client.userName)
    client.clientActor ! RefreshStoryboardsRequest(client.userName)

  }
}

object GetContourTreeAreasResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val parseAreas = ((s: String) => tokenize(s).map(_.toDouble).toArray)
  val parseDSSP = ((s: String) => {
      tokenize(s) match {
        case List("Some", rest) => Some(tokenize(rest).map(_.head).toArray)
        case List("None") => None
      }
    })
  val parseFiles = ((s: String)) => tokenize(s).map(new File(_)).toArray
  makeUnpickler(iso, parseAreas :: parseDSSP :: parseFiles :: HNil)
}
