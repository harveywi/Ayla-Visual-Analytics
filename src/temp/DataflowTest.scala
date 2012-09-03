package temp

import akka.dispatch.{ ExecutionContext, Future, Promise }
import akka.dispatch.Future.flow
import akka.actor.ActorSystem
import akka.dispatch.Await
import akka.util.duration._
import akka.actor.Actor
import javax.swing.SwingUtilities
import akka.actor.Props
import scala.util.continuations.cpsParam
import edu.osu.compgeom.ayla.DataflowProgressMonitor
import edu.osu.compgeom.ayla.HexProgressMonitor2

object DataflowTest {
//  def main(args: Array[String]): Unit = {
//    val actorSystem = ActorSystem("TestingCrap")
//    implicit val ec = ExecutionContext.defaultExecutionContext(actorSystem)
//
////    class MyStat(actorSystem: ActorSystem, private[this] val numStatusMessagesIn: Int = 4) extends {
////      override val system = actorSystem
////      override val numStatusMessages = numStatusMessagesIn
////    } with DataflowProgressMonitor {
////      def onProgressUpdate(prog: Double, statusMessages: Array[String]) = {
////        val p = prog * 100
////        println("Progress is %.2f".format(p))
////        statusMessages.foreach(s => println("\t" + s))
////      }
////    }
////
////    val status = new MyStat(actorSystem, 2)
//    
//    val status = HexProgressMonitor2(actorSystem, "Running crap", isIndeterminate = false)
//
//    val a = Promise[Int]
//    val b = Promise[Int]
////    val c = Promise[Int]
//    val d = Promise[Int]
//
////    status.add("Computing c") {
////      println("Blah")
////      c << a() + b() + d() + a() + a() + a()
////      println("Hello")
////    }
//
//    status.add("Computing a") {
//      Thread.sleep(1000)
//      a << 1
//    }
//
//    status.add("Computing d") {
//      Thread.sleep(200)
//      d << 3
//      println("d")
//    }
//
//    status.add("Computing b") {
//      b << 2
//      println("b")
//    }
//
//    val n = 100000
//    val myPromises = Array.fill(n)(Promise[Int])
//
//    myPromises.indices.foreach { x =>
//      status.add("Computing x for " + x) {
////                        Thread.sleep((math.random * 100).toInt)
//        myPromises(x) << x
//      }
//    }
//
//    status.goSeq
//    println("Waiting for stuff to cool down")
//
//    println("Summing promises")
//    val promiseSum = Future.reduce(myPromises)(_ + _)
//    val sum = Await.result(promiseSum, 1 day)
//    println("Sum is:  " + sum)
//    println("Sum should be:  " + ((n * (n + 1)) / 2 - n))
//
////        Thread.sleep(10000)
//
//    System.exit(0)
//  }
}