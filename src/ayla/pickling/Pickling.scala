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
  object JustATest {
    implicit def iso = Iso.hlist(JustATest.apply _, JustATest.unapply _)
    makeUnpickler(iso, parse(_.toInt) :: parse(_.toString) :: parse(_.toFloat) :: HNil)
  }

  val t = JustATest(11, "twenty two", 33.0f)
  val pickle = t.pickle
  println("Pickled:  " + pickle)

   PickleRegistry.unpickle(pickle) match {
    case Some(t) =>
      println("Success!")
      println(t)
    case None => println("Failure 1")
  }

  case class Test2(a: String, b: Int, c: Double) extends CanPickle[Test2]
  object Test2 {
    implicit def iso = Iso.hlist(Test2.apply _, Test2.unapply _)
    makeUnpickler(iso, parse(_.toString) :: parse(_.toInt) :: parse(_.toDouble) :: HNil)
  }

  val t2 = Test2("Hello world", 42, 100.0)
  val pickle2 = t2.pickle
  println("Pickled 2:  " + pickle2)
  
  PickleRegistry.unpickle(pickle) match {
    case Some(x) => println("Success 1")
    	println(x)
    case None => println("Failure 1")
  }
  
  PickleRegistry.unpickle(pickle2) match {
    case Some(x) => println("Success 2")
    	println(x)
    case None => println("Failure 2")
  }
  
  case class Test3(s: String) extends CanPickle[Test3]
  object Test3 extends CanUnpickle(parse(_.toString) :: HNil) {
    type CaseClass = Test3
    implicit def iso = Iso.hlist(Test3.apply _, Test3.unapply _)
    PickleRegistry.register(this.unpickle(_))
  }
  
  val t3 = Test3("Foo")
  val pickle3 = t3.pickle
  println("Pickled 3:  " + pickle3)
  
  case class Test4(x: Double, test3: Test3) extends CanPickle[Test4]
  object Test4 extends CanUnpickle(parse(_.toDouble) :: ((s: String) => Test3.unpickle(s).get) :: HNil) {
    type CaseClass = Test4
    implicit def iso = Iso.hlist(Test4.apply _, Test4.unapply _)
    PickleRegistry.register(this.unpickle(_))
  }
  
  val t4 = Test4(3.1415, t3)
  val pickle4 = t4.pickle
  println("Pickled 4:  " + pickle4)
  
  PickleRegistry.unpickle(pickle3) match {
    case Some(x) => println("Success 3:  " + x)
    case None => println("Failure 3")
  }
  
  PickleRegistry.unpickle(pickle4) match {
    case Some(x) => println("Success 4:  " + x)
    	println("the foo contains:  " + x.asInstanceOf[Test4].test3.s)
    case None => println("Failure 4")
  }
  
  case class Test5(stuff: List[Int]) extends CanPickle[Test5]
  object Test5 {
    implicit def iso = Iso.hlist(Test5.apply _, Test5.unapply _)
    makeUnpickler(iso, ((s: String) => tokenize(s).map(_.toInt)) :: HNil)
  }
  val t5 = Test5(List(1, 22, 333))
  val pickle5 = t5.pickle
  println("Pickle 5:  " + pickle5)
  
  PickleRegistry.unpickle(pickle5) match {
    case Some(x) => println("Success 5:  " + x)
    case None => println("Failure 5.")
  }
  
  case class Test6(stuff: Option[Int]) extends CanPickle[Test6]
  object Test6 {
    implicit val iso = Iso.hlist(apply _, unapply _)
    makeUnpickler(iso, ((s: String) => {
      tokenize(s) match {
        case List("Some", rest) => Some(rest.toInt)
        case List("None") => None
      }
    }) :: HNil)
  }
  val t6 = Test6(Some(123))
  val pickle6 = t6.pickle
  println("Pickle 6:  " + pickle6)
  PickleRegistry.unpickle(pickle6) match {
    case Some(x) => println("Success 6:  " + x)
    case None => println("Failure 6.")
  }
  
}