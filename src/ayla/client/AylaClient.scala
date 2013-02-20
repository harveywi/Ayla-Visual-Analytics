/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client

import java.io._
import scala.concurrent.Await
import scala.concurrent.util.duration.intToDurationInt
import scala.swing.Publisher
import scala.util.matching.Regex
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import ayla.client.ui.LoginDialog
import ayla.collab._
import ayla.geometry.ScalarFunction
import ayla.geometry.ct.ContourTree
import ayla.protocol._
import reactive._
import java.net.Socket

class AylaClient extends Publisher with Observing {
  val collabFrame = new AylaCollaborationFrame(this)
  var userName: String = ""
  var socket: Socket = null
  var ct: ContourTree = null
  var sf: ScalarFunction = null

  val actorSystem = ActorSystem("AylaServerSystem")
  implicit val ec = actorSystem.dispatcher
  val clientActor = actorSystem.actorOf(Props(new ClientActor(this)))

  def connectToServer() = LoginDialog.getLoginInfo match {
    case Some(loginInfo) =>
      clientActor ! loginInfo
    case None => System.exit(0)
  }

  def requestAndReact[T <: Serializable, R <: MsgFromServer](msg: MsgFromClient[R], es: EventSource[T]): T = {
    class Foo extends Actor {
      def receive = {
        case "WaitForResult" =>
          val s = sender
          context.become {
            case x => s ! x
          }
      }
    }

    implicit val timeout = Timeout(2000.hours)
    val foo = actorSystem.actorOf(Props(new Foo))

    // The 'done' flag allows the event stream to be garbage collected.
    var done = false
    es.takeWhile(_ => !done).foreach { t =>
      foo ! t
      done = true
    }
    val future = foo ? "WaitForResult"
    clientActor ! msg

    println("Waiting for something to happen...")
    val ret = Await.result(future, timeout.duration).asInstanceOf[T]
    foo ! PoisonPill                                                          
    ret
  }

  def getPDBLines(iSampled: Int): Array[String] = requestAndReact(
    new GetPDBLinesRequest(userName, iSampled), EventStreams.pdbLines)

  def getColorFunction(f: File): Array[Float] = requestAndReact(
    GetColorFunctionRequest(userName, f), EventStreams.colorFunction)

  def postAnnotation(annotation: ConformationAnnotation): Unit = clientActor ! new CreateAnnotationRequest(userName, annotation)

  def findMatchingConformations(regex: Regex): Array[(String, Int)] = requestAndReact(
    FindMatchingConformationsRequest(userName, regex), EventStreams.matchingConformations)

  def getContactDensities(residues: Array[Int]): Array[Int] = requestAndReact(
    GetContactDensitiesRequest(userName, residues), EventStreams.contactDensities)

  def getDomainShortestPath(idStart: Int, idEnd: Int): Array[Int] = requestAndReact(
    GetDomainShortestPathRequest(userName, idStart, idEnd), EventStreams.domainShortestPath)

  def postStoryboard(storyboard: Storyboard): Unit = clientActor ! CreateStoryboard(userName, storyboard)
  def postChatMessage(text: String): Unit = clientActor ! new CreateChatMessage(userName, text)

  object EventStreams {
    val pdbLines = new EventSource[Array[String]]
    val dsspOutput = new EventSource[Option[Array[Char]]]
    val colorFunctions = new EventSource[Array[File]]
    val colorFunction = new EventSource[Array[Float]]
    val matchingConformations = new EventSource[Array[(String, Int)]]
    val contactDensities = new EventSource[Array[Int]]
    val domainShortestPath = new EventSource[Array[Int]]
  }

}

object AylaClient extends App {
  val client = new AylaClient
  client.connectToServer()
}
