package ayla.pickling

import shapeless._
import scala.language.implicitConversions
import java.io.File

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
  def p[T](t: T): String = {
      val str = t.toString
      s"${str.length} $str"
    }
  
  object toPickle extends Poly1 {
    implicit def caseDouble = at[Double](p)
    implicit def caseFloat = at[Float](p)
    implicit def caseLong = at[Long](p)
    implicit def caseInt = at[Int](p)
    implicit def caseShort = at[Short](p)
    implicit def caseByte = at[Byte](p)
    implicit def caseBoolean = at[Boolean](p)
    implicit def caseChar = at[Char](p)
    implicit def caseString = at[String](p)
    implicit def caseFile = at[File](f => p(f.getAbsolutePath))
    
    implicit def caseOption[T](implicit c: Case1[T]) = at[Option[T]]{opt =>
      val encoded = opt match {
        case Some(t) =>
          p("Some") + toPickle(t)
        case None => p("None")
      }
      p(encoded)
    }
    
    implicit def caseMap[A, B](implicit ca: Case1[A], cb: Case1[B]) = at[Map[A, B]]{map =>
      val encoded = map.flatMap{case (key, value) =>
        Seq(toPickle(key), toPickle(value))
      }.mkString
      p(encoded)
    }
    
    implicit def caseRegex = at[scala.util.matching.Regex](p)
    implicit def caseArrayTupleStringInt = at[Array[(String, Int)]]{arr =>
      val encoded = arr.flatMap{case (s, i) => Seq(p(s), p(i))}.mkString
      p(encoded)
    }
    
    implicit def caseList[T](implicit c: Case1[T]) = at[List[T]]{trav =>
      val encoded = trav.map(toPickle).mkString
      p(encoded)
    }
    
    implicit def caseArray[T](implicit c: Case1[T]) = at[Array[T]]{arr =>
      val encoded = arr.map(toPickle).mkString
      p(encoded)
    }
    
    implicit def caseCanPickle[T <: CanPickle[T], H1 <: HList](implicit iso: Iso[T, H1], mapFolder: MapFolder[H1, String, toPickle.type]) = at[T]{x =>
      p(x.pickle)
    }
  }
}