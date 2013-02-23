package ayla.pickling2

import scala.util.matching.Regex
import java.io.{File, DataInputStream}
import scala.reflect.ClassTag

trait DefaultUnpicklers {
  
	implicit val stringUnpickler: Unpickler[String] = new Unpickler[String] {
		def unpickle(dais: DataInputStream) = dais.readUTF
	}
	
	implicit val intUnpickler: Unpickler[Int] = new Unpickler[Int] {
		def unpickle(dais: DataInputStream) = dais.readInt
	}
	
	implicit val boolUnpickler: Unpickler[Boolean] = new Unpickler[Boolean] {
	  def unpickle(dais: DataInputStream) = dais.readBoolean
	}
	
	implicit val floatUnpickler = new Unpickler[Float] {def unpickle(dais: DataInputStream) = dais.readFloat}
	
	implicit val doubleUnpickler = new Unpickler[Double] {def unpickle(dais: DataInputStream) = dais.readDouble}
	
	implicit val regexUnpickler = new Unpickler[Regex] {def unpickle(dais: DataInputStream) = new Regex(dais.readUTF)}
	
	implicit val fileUnpickler = new Unpickler[File] {def unpickle(dais: DataInputStream) = new File(dais.readUTF)}
	
	implicit val charUnpickler = new Unpickler[Char] {def unpickle(dais: DataInputStream) = dais.readChar}
	
	implicit def optionUnpickler[T](implicit unpickler: Unpickler[T]): Unpickler[Option[T]] = new Unpickler[Option[T]] {
//	  def unpickle(s: String) = {
//	    Some(if (s.startsWith("Some")) {
//	      unpickler.unpickle(s.drop(4))
//	    } else {
//	      None
//	    })
//	  }
	  def unpickle(dais: DataInputStream) = {
	    if (dais.readUTF == "Some") {
	      Some(unpickler.unpickle(dais))
	    } else {
	      None
	    }
	  }
	}
	
	implicit def listUnpickler[T](implicit elementUnpickler: Unpickler[T]): Unpickler[List[T]] = new Unpickler[List[T]] {
//	  def unpickle(s: String) = {
//	    val tokens = Unpickler.tokenize(s)
//	    Some(tokens.map(elementUnpickler.unpickle(_).get))
//	  }
	  def unpickle(dais: DataInputStream) = {
	    val n = dais.readInt
	    (for (_ <- 1 to n) yield elementUnpickler.unpickle(dais)).toList
	  }
  }
	
	implicit def mapUnpickler[K, V](implicit keyUnpickler: Unpickler[K], valUnpickler: Unpickler[V]): Unpickler[Map[K, V]] = new Unpickler[Map[K, V]] {
	  def unpickle(dais: DataInputStream) = {
	    listUnpickler(tuple2Unpickler(keyUnpickler, valUnpickler)).unpickle(dais).toMap
	  }
	}
	
	implicit def tuple2Unpickler[T1, T2](implicit unpickler1: Unpickler[T1], unpickler2: Unpickler[T2]): Unpickler[(T1, T2)] = new Unpickler[(T1, T2)] {
	  def unpickle(dais: DataInputStream) = {
	    (unpickler1.unpickle(dais), unpickler2.unpickle(dais))
	  }
	}
	
	implicit def arrayUnpickler[T](implicit m: ClassTag[T], elementUnpickler: Unpickler[T]): Unpickler[Array[T]] = new Unpickler[Array[T]] {
	  def unpickle(dais: DataInputStream) = {
	    val n = dais.readInt
	    (for (_ <- 1 to n) yield elementUnpickler.unpickle(dais)).toArray
//	    val tokens = Unpickler.tokenize(s)
//	    Some(tokens.flatMap(t => elementUnpickler.unpickle(t)).toArray)
	  }
	}
	
}

object DefaultUnpicklers extends DefaultUnpicklers