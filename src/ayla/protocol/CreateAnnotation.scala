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
import scala.collection._
import ayla.collab.ConformationAnnotation

import ayla.pickling._
import ayla.pickling.CanUnpickle._
import shapeless._
import shapeless.Functions._

case class CreateAnnotationRequest(username: String, annotation: ConformationAnnotation) extends MsgFromClient[CreateAnnotationResponse] with CanPickle[CreateAnnotationRequest] {
  def serverDo[H1 <: HList](server: AylaServer, oosServer: ObjectOutputStream)(implicit iso: Iso[CreateAnnotationResponse, H1],
      mapFolder: MapFolder[H1, String, CanPickle.toPickle.type]) = server.logAnnotation(username, annotation)
}

object CreateAnnotationRequest {
  implicit def iso = Iso.hlist(CreateAnnotationRequest.apply _, CreateAnnotationRequest.unapply _)
  val parser = parse(_.toString) :: ((s: String) => ConformationAnnotation.unpickle(s).get) :: HNil 
  makeUnpickler(iso,  parser)
}

case class CreateAnnotationResponse(r: String) extends MsgFromServer {
   def clientDo(client: AylaClient, oosReply: ObjectOutputStream): Unit = {}
}
object CreateAnnotationResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
}