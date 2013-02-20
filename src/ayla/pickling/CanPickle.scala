package ayla.pickling

import shapeless._

trait CanPickle[T] { self: T =>
  import CanPickle._
  
//  type H1 <: HList
//  implicit def iso: Iso[T, H1] = implicitly[Iso[T, H1]]
//  implicit def mapFolder: MapFolder[H1, String, toPickle.type] = implicitly[MapFolder[H1, String, toPickle.type]]
//  
//  def pickle: String = {
//    val h1 = iso.to(this)
//    val className = this.getClass.getName
//
//    s"${className.length} $className" + h1.foldMap("")(toPickle)(_ + _)
//  }
  
  def pickle[H1 <: HList](implicit iso: Iso[T, H1], mapFolder: MapFolder[H1, String, toPickle.type]): String = {
    val h1 = iso.to(this)
    val className = this.getClass.getName

    s"${className.length} $className" + h1.foldMap("")(toPickle)(_ + _)
  }

}

object CanPickle {
  object toPickle extends Poly1 {
    def p[T](t: T): String = {
      val str = t.toString
      s"${str.length} $str"
    }

    implicit def caseDouble = at[Double](p)
    implicit def caseFloat = at[Float](p)
    implicit def caseLong = at[Long](p)
    implicit def caseInt = at[Int](p)
    implicit def caseShort = at[Short](p)
    implicit def caseByte = at[Byte](p)
    implicit def caseBoolean = at[Boolean](p)
    implicit def caseChar = at[Char](p)
    implicit def caseString = at[String](p)
    implicit def caseCanPickle[T <: CanPickle[T], H1 <: HList](implicit iso: Iso[T, H1], mapFolder: MapFolder[H1, String, toPickle.type]) = at[T]{x =>
      p(x.pickle)
    }
  }
}