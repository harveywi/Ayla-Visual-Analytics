package ayla.pickling2

import scala.reflect.ClassTag
import scala.util.matching.Regex
import java.io.File
import akka.util.ByteString
import akka.util.ByteStringBuilder
import java.io.DataOutputStream

trait DefaultPicklers {
  
  def pickled[T](t: T, daos: DataOutputStream)(implicit p: Pickler[T]): Unit = p.pickle(t, daos)
  
  implicit val intPickler: Pickler[Int] = new Pickler[Int] {
    def pickle(i: Int, daos: DataOutputStream) = daos.writeInt(i)
  }

  implicit val stringPickler: Pickler[String] = new Pickler[String] {
    def pickle(s: String, daos: DataOutputStream) = daos.writeUTF(s)
  }
  
  implicit val booleanPickler = new Pickler[Boolean] {
    def pickle(b: Boolean, daos: DataOutputStream) = daos.writeBoolean(b)
  }
  
  implicit val floatPickler = new Pickler[Float] {
    def pickle(f: Float, daos: DataOutputStream) = daos.writeFloat(f)
  }
  
  implicit val doublePickler = new Pickler[Double] {
    def pickle(d: Double, daos: DataOutputStream) = daos.writeDouble(d)
  }
  
  implicit def regexPickler = new Pickler[Regex] {
    def pickle(r: Regex, daos: DataOutputStream) = daos.writeUTF(r.toString)
  }
  
  implicit def charPickler = new Pickler[Char] {
    def pickle(c: Char, daos: DataOutputStream) = daos.writeChar(c)
  }
  
  implicit def filePickler = new Pickler[File] {
    def pickle(f: File, daos: DataOutputStream) = daos.writeUTF(f.getAbsolutePath)
  }
  
  implicit def optionPickler[T](implicit pickler: Pickler[T]): Pickler[Option[T]] = new Pickler[Option[T]] {
    def pickle(tOpt: Option[T], daos: DataOutputStream) = tOpt match {
      case Some(t) =>
        daos.writeUTF("Some")
        pickler.pickle(t, daos)
      case None =>
        daos.writeUTF("None")
    }
  }
  
  implicit def tuple2Pickler[T1, T2](implicit pickler1: Pickler[T1], pickler2: Pickler[T2]): Pickler[(T1, T2)] = new Pickler[(T1, T2)] {
    def pickle(t: (T1, T2), daos: DataOutputStream) = {
      pickler1.pickle(t._1, daos)
      pickler2.pickle(t._2, daos)
    }
  }
  
  implicit def arrayPickler[T : ClassTag](implicit elementPickler: Pickler[T]): Pickler[Array[T]] = new Pickler[Array[T]] {
    def pickle(arr: Array[T], daos: DataOutputStream) = {
      daos.writeInt(arr.length)
      arr.foreach(x => elementPickler.pickle(x, daos))
    }
  }
  
  implicit def seqPickler[T](implicit elementPickler: Pickler[T]): Pickler[Seq[T]] = new Pickler[Seq[T]] {
    def pickle(seq: Seq[T], daos: DataOutputStream) = {
      daos.writeInt(seq.size)
      seq.foreach(x => elementPickler.pickle(x, daos))
    }
  }
  
  implicit def mapPickler[K, V](implicit keyPickler: Pickler[K], valuePickler: Pickler[V]): Pickler[Map[K, V]] = new Pickler[Map[K, V]] {
    def pickle(m: Map[K, V], daos: DataOutputStream) = {
      seqPickler(tuple2Pickler(keyPickler, valuePickler)).pickle(m.toSeq, daos)
    }
  }
}

object DefaultPicklers extends DefaultPicklers