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

import shapeless._
import ayla.pickling2.Pickling._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2

case class CreateAnnotationRequest(username: String, annotation: ConformationAnnotation) extends MsgFromClient {
  def pickled(daos: java.io.DataOutputStream) = CreateAnnotationRequest.pickler.pickle(this, daos)
  def serverDo(server: AylaServer, oosServer: DataOutputStream) = server.logAnnotation(username, annotation)
}

object CreateAnnotationRequest {
  implicit def iso = Iso.hlist(CreateAnnotationRequest.apply _, CreateAnnotationRequest.unapply _)
  implicit def iso2 = ConformationAnnotation.iso
  implicit val (p, u) = picklerUnpickler[ConformationAnnotation].create()
  
  val (pickler, unpickler) = picklerUnpickler[CreateAnnotationRequest].create()
  PicklerRegistry2.register(picklerUnpickler[CreateAnnotationRequest].create())
}
