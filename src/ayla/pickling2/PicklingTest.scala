package ayla.pickling2

import shapeless._
import Pickling._
import DefaultPicklers._
import DefaultUnpicklers._

object PicklingTest extends App {
  case class Test1(x: String, y: Int)
  implicit val iso1 = Iso.hlist(Test1.apply _, Test1.unapply _)
  val pickler1 = pickler[Test1].create()
  val t1 = Test1("Hello", 42)
  val pickle1 = pickler1.pickle(t1)
  println("Pickle 1:  " + pickle1)
  val unpickler1 = unpickler[Test1].create()
  unpickler1.unpickle(pickle1) match {
    case Some(x) => println("Success 1:  " + x)
    case None => System.err.println("Failure 1")
  }
  println()
  
  case class Test2(x: String, a1: Test1, a2: Test1)
  implicit val iso2 = Iso.hlist(Test2.apply _, Test2.unapply _)
  implicit val test1pickler = pickler[Test1].create()
  implicit val test1unpickler = unpickler[Test1].create()
  val pickler2 = pickler[Test2].create()
  val t2 = Test2("Hello", Test1("World", 400), Test1("Bippy", 1000))
  val pickle2 = pickler2.pickle(t2)
  println("Pickle 2:  " + pickle2)
  val unpickler2 = unpickler[Test2].create()
  unpickler2.unpickle(pickle2) match {
    case Some(x) => println("Success 2:  " + x)
    case None => System.err.println("Failure 2")
  }
  
  val pu = picklerUnpickler[Test2].create()
  println(pu._2.unpickle(pu._1.pickle(t2)))
  
  PicklerRegistry2.register(pu)
  println(PicklerRegistry2.unpickle(pickle1))
  
}