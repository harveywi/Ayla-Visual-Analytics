package ayla.pickling

import scala.language.implicitConversions
import shapeless._
import shapeless.Functions._
import shapeless.Traversables._
import shapeless.Nat._
import scala.annotation.tailrec

object Pickling extends App {

  import CanUnpickle._
  case class JustATest(x: Int, y: String, z: Float) extends CanPickle[JustATest]
  object JustATest extends CanUnpickle(parse(_.toInt) :: parse(_.toString) :: parse(_.toFloat) :: HNil) {
    type CaseClass = JustATest
    implicit def iso = Iso.hlist(JustATest.apply _, JustATest.unapply _)
  }

  val t = JustATest(11, "twenty two", 33.0f)
  val pickle = t.pickle
  println("Pickled:  " + pickle)

  JustATest.unpickle(pickle) match {
    case Some(t) =>
      println("Success!")
      println(t)
    case None => println("Failure")
  }
  
  case class Test2(a: String, b: Int, c: Double) extends CanPickle[Test2]
  object Test2 extends CanUnpickle(
      parse(_.toString) :: parse(_.toInt) :: parse(_.toDouble) :: HNil
  ) {
    type CaseClass = Test2
    implicit def iso = Iso.hlist(Test2.apply _, Test2.unapply _)
  }
  val t2 = Test2("Hello world", 42, 100.0)
  val pickle2 = t2.pickle
  println("Pickled 2:  " + pickle2)
  
//  Test2.unpickle(pickle2) match {
//    case Some(t) =>
//      println("Success!")
//      println(t)
//    case None => println("Failure")
//  }
//  
//  Test2.unpickle(pickle) match {
//    case Some(t) => println("***Success")
//    case None => println("***FAIL")
//  }
  
  
}