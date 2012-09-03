package edu.osu.compgeom.ayla.message

import edu.osu.compgeom.ayla._
import java.io._
import akka.actor._
import akka.pattern.ask
import akka.util.duration._
import akka.util.Timeout
import akka.dispatch.{ Future, Await }

//abstract class RequestResponse[Req: ClassManifest, Res: ClassManifest] {
//  def clientReactions: Actor.Receive = {
//    case value if classManifest[Req].erasure.isInstance(value) => println("A")
//  }
//
//  val serverReactions: PartialFunction[Any, Unit] = {
//    case value if classManifest[Res].erasure.isInstance(value) => println("B")
//  }
//
//  def sendRequest(req: Req)
//
//  def sendResponse(res: Res)
//}

class ReqResProtocol[A: ClassManifest, B] {
  
  def clientReceive(out: ObjectOutputStream): PartialFunction[Any, Unit] = {
    //    case x => println("got something:  " + x)
    case e if classManifest[A].erasure.isInstance(e) => {
      println("Yay!")
      //      out.writeObject(e)
      //      //        out.flush()
      //      //        val replyTo = sender
      //      //        become {
      //      //          case RegisterUserNameResponse => {
      //      //            replyTo ! RegisterUserNameResponse
      //      //            unbecome()
      //      //          }
      //      //        }
    }
  }

}

//trait HasReceive extends Actor {
////  def receive: Actor.Receive = {
////    case msg => throw new RuntimeException("Received an unhandled message:  " + msg)
////  }
//  def receive: Actor.Receive = Actor.emptyBehavior
//}

trait BaseClientActor extends Actor {
  def receive: Actor.Receive = Actor.emptyBehavior
  
  def sendAndWait[A, B](a: A, sender: ActorRef)(implicit m: Manifest[B]): B = {
    implicit val timeout = Timeout(1 minute)
    val future = ask(sender, a).mapTo[B]
    Await.result[B](future, timeout.duration)
  }
  
}

object ColorFunctions extends ReqResProtocol[GetColorFunctionsRequest.type, GetColorFunctionsResponse] {
  trait Foo extends BaseClientActor { self: MyClient =>
    def getColorFunctions(): Array[(File, String)] = sendAndWait[GetColorFunctionsRequest.type, GetColorFunctionsResponse](GetColorFunctionsRequest, null).functions
    //    implicit val timeout = Timeout(1 minute)
    //    val future = clientActor ? GetColorFunctionsRequest
    //    val result = Await.result(future, timeout.duration).asInstanceOf[GetColorFunctionsResponse]
    //    result.functions
    //  }

    override def receive: Actor.Receive = super.receive orElse clientReceive(null)
  }

  trait Bar { self: MyServer =>
    def nooo = "Nooo!"
  }
}

object CollabProjects extends ReqResProtocol[GetCollabProjectRequest, GetCollabProjectResponse] {
  trait Foo extends BaseClientActor { self: MyClient =>
    override def receive: Actor.Receive = super.receive orElse {
      case e: Int => println("Int:  " + e)
    }
  }

  trait Bar { self: MyServer =>
    def nooo = "Nooo!"
  }
}

class MyClient

class MyServer {

}

object RequestResponse extends App {
  println("Hello world")
  val system = ActorSystem("AylaServerSystem")
  val ac = system.actorOf(Props(new MyClient with ColorFunctions.Foo with CollabProjects.Foo))

  ac ! GetColorFunctionRequest(null)

  val as = new MyServer
}