package ayla.pickling2

import shapeless._
import Pickling._
import DefaultPicklers._
import DefaultUnpicklers._

object PicklingTest extends App {
//  case class Test1(x: String, y: Int)
//  implicit val iso1 = Iso.hlist(Test1.apply _, Test1.unapply _)
//  val pickler1 = pickler[Test1].create()
//  val t1 = Test1("Hello", 42)
//  val pickle1 = pickler1.pickle(t1)
//  println("Pickle 1:  " + pickle1.decodeString("UTF-8"))
//  val unpickler1 = unpickler[Test1].create()
//  unpickler1.unpickle(pickle1.decodeString("UTF-8")) match {
//    case Some(x) => println("Success 1:  " + x)
//    case None => System.err.println("Failure 1")
//  }
//  println()
//  
//  case class Test2(x: String, a1: Test1, a2: Test1)
//  implicit val iso2 = Iso.hlist(Test2.apply _, Test2.unapply _)
//  implicit val test1pickler = pickler[Test1].create()
//  implicit val test1unpickler = unpickler[Test1].create()
//  val pickler2 = pickler[Test2].create()
//  val t2 = Test2("Hello", Test1("World", 400), Test1("Bippy", 1000))
//  val pickle2 = pickler2.pickle(t2)
//  println("Pickle 2:  " + pickle2.decodeString("UTF-8"))
//  val unpickler2 = unpickler[Test2].create()
//  unpickler2.unpickle(pickle2.decodeString("UTF-8")) match {
//    case Some(x) => println("Success 2:  " + x)
//    case None => System.err.println("Failure 2")
//  }
//  
//  val pu = picklerUnpickler[Test2].create()
//  println(pu._2.unpickle(pu._1.pickle(t2).decodeString("UTF-8")))
//  
//  case class Test3(x: Array[Char])
//  implicit val iso3 = Iso.hlist(Test3.apply _, Test3.unapply _)
//  val (pickler3, unpickler3) = picklerUnpickler[Test3].create()
//  val t3 = Test3(Array('a', 'b', 'c'))
//  val pickle3 = pickler3.pickle(t3)
//  println("Pickle 3:  " + pickle3.decodeString("UTF-8"))
//  unpickler3.unpickle(pickle3.decodeString("UTF-8")) match {
//    case Some(Test3(arr)) => println("Success 3:  " + arr.mkString(","))
//    case None => System.err.println("Failure 3")
//  }
//  
//  import java.io._
//  
//  val baos = new ByteArrayOutputStream
//  val daos = new DataOutputStream(baos)
//  daos.writeInt(42)
//  daos.writeUTF("Hello world")
//  
//  val bais = new ByteArrayInputStream(baos.toByteArray)
//  val puis = new BufferedInputStream(bais)
//  val dais = new DataInputStream(puis)
//  
//  puis.mark(Int.MaxValue)
//  println(dais.readInt)
//  puis.reset()
//  puis.mark(Int.MaxValue)
//  println(dais.readInt)
//  puis.reset()
//  println(dais.readInt)
//  println(dais.readUTF())
  
}