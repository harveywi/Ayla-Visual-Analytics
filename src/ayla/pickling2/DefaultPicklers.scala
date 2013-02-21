package ayla.pickling2

import scala.reflect.ClassTag
import scala.util.matching.Regex
import java.io.File

trait DefaultPicklers {
  def p[T](t: T): String = {
    val str = t.toString
    s"${str.length} $str"
  }
  
  implicit val intPickler: Pickler[Int] = new Pickler[Int] {
    def pickle(i: Int): String = p(i)
  }

  implicit val stringPickler: Pickler[String] = new Pickler[String] {
    def pickle(s: String): String = p(s)
  }
  
  implicit val booleanPickler = new Pickler[Boolean] {
    def pickle(b: Boolean) = p(b)
  }
  
  implicit val floatPickler = new Pickler[Float] {
    def pickle(f: Float) = p(f)
  }
  
  implicit val doublePickler = new Pickler[Double] {
    def pickle(d: Double) = p(d)
  }
  
  implicit def regexPickler = new Pickler[Regex] {
    def pickle(r: Regex) = p(r)
  }
  
  implicit def charPickler = new Pickler[Char] {
    def pickle(c: Char) = p(c)
  }
  
  implicit def filePickler = new Pickler[File] {
    def pickle(f: File) = p(f.getAbsolutePath)
  }
  
  implicit def optionPickler[T](implicit pickler: Pickler[T]): Pickler[Option[T]] = new Pickler[Option[T]] {
    def pickle(tOpt: Option[T]) = tOpt match {
      case Some(t) =>
        p("Some" + pickler.pickle(t))
      case None =>
        p("None")
    }
  }
  
  implicit def tuple2Pickler[T1, T2](implicit pickler1: Pickler[T1], pickler2: Pickler[T2]): Pickler[(T1, T2)] = new Pickler[(T1, T2)] {
    def pickle(t: (T1, T2)) = {
      p(pickler1.pickle(t._1) + pickler2.pickle(t._2))
    }
  }
  
  implicit def arrayPickler[T : ClassTag](implicit elementPickler: Pickler[T]): Pickler[Array[T]] = new Pickler[Array[T]] {
    def pickle(arr: Array[T]): String = {
      val encoded = arr.map(elementPickler.pickle _).mkString
      p(encoded)
    }
  }
  
  implicit def seqPickler[T](implicit elementPickler: Pickler[T]): Pickler[Seq[T]] = new Pickler[Seq[T]] {
    def pickle(seq: Seq[T]): String = p(seq.map(elementPickler.pickle(_)).mkString)
  }
  
  implicit def mapPickler[K, V](implicit keyPickler: Pickler[K], valuePickler: Pickler[V]): Pickler[Map[K, V]] = new Pickler[Map[K, V]] {
    def pickle(m: Map[K, V]): String = {
      seqPickler(tuple2Pickler(keyPickler, valuePickler)).pickle(m.toSeq)
    }
  }
}

object DefaultPicklers extends DefaultPicklers