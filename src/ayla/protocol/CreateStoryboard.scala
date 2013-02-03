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

case class CreateStoryboard(username: String, storyboard: Storyboard) extends MsgFromClient {
  def serverDo(server: AylaServer, oosServer: ObjectOutputStream) = server.logStoryboard(username, storyboard)
}
