/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui

import akka.actor._
import akka.dispatch._
import scala.concurrent.{Promise, Future, ExecutionContext}

trait DataflowProgressMonitor {
  implicit val ec: ExecutionContext
  def onStart = {}
  def onDone = {}
  def onProgressUpdate(progress: Double, statusMessages: Array[String]): Unit

  val system: ActorSystem
  val numStatusMessages: Int

  private[this] class StatusActor extends Actor {
    def receive = {
      case Start(msg, taskID) => {
        statusMessages.enqueue("[STARTING] " + msg)
        if (statusMessages.size > numStatusMessages)
          statusMessages.dequeue()
        val progress = 1 - (promises.size - numTasksCompleted) / promises.size.toDouble
        onProgressUpdate(progress, statusMessages.toArray)
      }

      case Done(msg, taskID) => {
        statusMessages.enqueue("[DONE] " + msg)
        if (statusMessages.size > numStatusMessages)
          statusMessages.dequeue()

        numTasksCompleted += 1
        val progress = 1 - (promises.size - numTasksCompleted) / promises.size.toDouble
        onProgressUpdate(progress, statusMessages.toArray)
        if (numTasksCompleted == promises.size) {
          onDone
        }
      }
    }
  }

  private[this] val statusMessages = new scala.collection.mutable.Queue[String]

  private[this] var numTasksCompleted = 0
  private[this] val statusActor = system.actorOf(Props(new StatusActor))

  private[this] sealed abstract class Msg
  private[this] case class Start(msg: String, taskID: Int) extends Msg
  private[this] case class Done(msg: String, taskID: Int) extends Msg

  private[this] val promises = new scala.collection.mutable.ArrayBuffer[(String, Promise[_])]
  
  def add[T](taskName: String, promise: Promise[T])(implicit ec: ExecutionContext): Promise[T] = {
    promises += new Tuple2(taskName, promise)

//    promise.future.onComplete {
//      case Right(result) => {
//        statusActor ! Done(taskName, 0)
//      }
//      case Left(error) => ErrorDialog.showErrorAndQuit(new Exception(error))
//    }

    promise
  }

  def add2(taskName: String) = new {
    def apply[T](task: => T): Promise[T] = {
      val promise = Promise[T]
      promises += new Tuple2(taskName, promise)

      val future = Future({
        statusActor ! Start(taskName, 0)
        task
      })
      promise.completeWith(future)
      
//      promise.future.onComplete {
//        case Right(result) => {
//          statusActor ! Done(taskName, 0)
//        }
//        case Left(error) => ErrorDialog.showErrorAndQuit(new Exception(error))
//      }

      promise
    }

    def apply[T](future: Future[T]): Promise[T] = {
      val promise = Promise[T]
      promises += new Tuple2(taskName, promise)
      
      Future({
        statusActor ! Start(taskName, 0)
      })
      
      promise.completeWith(future)
      
//      promise.future.onComplete {
//        case Right(result) => {
//          statusActor ! Done(taskName, 0)
//        }
//        case Left(error) => ErrorDialog.showErrorAndQuit(new Exception(error))
//      }
      
      promise
    }
  }
}
