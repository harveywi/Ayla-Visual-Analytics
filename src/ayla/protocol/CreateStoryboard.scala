/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.protocol

import ayla.collab.Storyboard
import java.io._
import ayla.server.AylaServer
import ayla.pickling._
import ayla.pickling.CanUnpickle._
import shapeless._
import shapeless.Functions._
import ayla.client.AylaClient

case class CreateStoryboard(username: String, storyboard: Storyboard) extends MsgFromClient[CreateStoryboardResponse] with CanPickle[CreateStoryboard] {
  def serverDo[H1 <: HList](server: AylaServer, oosServer: ObjectOutputStream)(implicit iso: Iso[CreateStoryboardResponse, H1],
      mapFolder: MapFolder[H1, String, CanPickle.toPickle.type]) = server.logStoryboard(username, storyboard)
}

object CreateStoryboard {
  implicit def iso = Iso.hlist(CreateStoryboard.apply _, CreateStoryboard.unapply _)
  makeUnpickler(iso,  parse(_.toString) :: ((s: String) => Storyboard.unpickle(s).get) :: HNil)
}

case class CreateStoryboardResponse(sb: String) extends MsgFromServer {
  def clientDo(client: AylaClient, oosReply: ObjectOutputStream): Unit = {}
}
object CreateStoryboardResponse {
  implicit def iso = Iso.hlist(apply _, unapply _)
}