package ayla.protocol

import ayla.client._
import ayla.server._
import java.io._

import shapeless._
import ayla.pickling2.Pickling._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2

case class GetDsspLabelsRequest(username: String, numResidues: Int, id: Int) extends MsgFromClient {
  def pickled(daos: java.io.DataOutputStream) = GetDsspLabelsRequest.pickler.pickle(this, daos)

  def serverDo(server: AylaServer, daosServer: DataOutputStream) = replyWith(daosServer) {
    val (proj, dataset) = server.userSessions.find(_.username == username).map { session => (session.collabProject, session.dataset) }.get
    val barLabels = Array.fill[Char](numResidues)('X')
    val dsspArray = dataset.dsspOutput.get
    (0 until numResidues).foreach { i =>
      val ofs = id * numResidues + i
      barLabels(i) = dsspArray(ofs)
    }
    GetDsspLabelsResponse(barLabels)
  }
}

object GetDsspLabelsRequest {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[GetDsspLabelsRequest].create()
}

case class GetDsspLabelsResponse(dsspLabels: Array[Char]) extends MsgFromServer {
  def pickled(daos: java.io.DataOutputStream) = GetDsspLabelsResponse.pickler.pickle(this, daos)
  def clientDo(client: AylaClient, daosClient: DataOutputStream) = {
    client.EventStreams.dsspLabels.fire(dsspLabels)
  }
}

object GetDsspLabelsResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[GetDsspLabelsResponse].create()
}