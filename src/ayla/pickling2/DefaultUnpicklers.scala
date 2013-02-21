package ayla.pickling2

import scala.util.matching.Regex
import java.io.File

trait DefaultUnpicklers {
  
	implicit val stringUnpickler: Unpickler[String] = new Unpickler[String] {
		def unpickle(s: String) = Some(s)
	}
	
	implicit val intUnpickler: Unpickler[Int] = new Unpickler[Int] {
		def unpickle(s: String) = Some(s.toInt)
	}
	
	implicit val boolUnpickler: Unpickler[Boolean] = new Unpickler[Boolean] {
	  def unpickle(s: String) = Some(s.toBoolean)
	}
	
	implicit val floatUnpickler = new Unpickler[Float] {def unpickle(s: String) = Some(s.toFloat)}
	
	implicit val doubleUnpickler = new Unpickler[Double] {def unpickle(s: String) = Some(s.toDouble)}
	
	implicit val regexUnpickler = new Unpickler[Regex] {def unpickle(s: String) = Some(new Regex(s))}
	
	implicit val fileUnpickler = new Unpickler[File] {def unpickle(s: String) = Some(new File(s))}
	
	implicit val charUnpickler = new Unpickler[Char] {def unpickle(s: String) = Some(s.head)}
	
	implicit def optionUnpickler[T](implicit unpickler: Unpickler[T]): Unpickler[Option[T]] = new Unpickler[Option[T]] {
	  def unpickle(s: String) = {
	    Some(if (s.startsWith("Some")) {
	      unpickler.unpickle(s.drop(4))
	    } else {
	      None
	    })
	  }
	}
	
	implicit def listUnpickler[T](implicit elementUnpickler: Unpickler[T]): Unpickler[List[T]] = new Unpickler[List[T]] {
	  def unpickle(s: String) = {
	    val tokens = Unpickler.tokenize(s)
	    Some(tokens.map(elementUnpickler.unpickle(_).get))
	  }
//    def validated(jvalue: JValue) = jvalue match {
//      case JArray(values) => values.map(elementExtractor.validated _).sequence[VE, T]
//      case _ => invalidv("Expected JArray but found: " + jvalue)
//    }
  }
	
	implicit def mapUnpickler[K, V](implicit keyUnpickler: Unpickler[K], valUnpickler: Unpickler[V]): Unpickler[Map[K, V]] = new Unpickler[Map[K, V]] {
	  def unpickle(s: String) = {
	    listUnpickler(tuple2Unpickler(keyUnpickler, valUnpickler)).unpickle(s).map(_.toMap)
	  }
	}
	
	implicit def tuple2Unpickler[T1, T2](implicit unpickler1: Unpickler[T1], unpickler2: Unpickler[T2]): Unpickler[(T1, T2)] = new Unpickler[(T1, T2)] {
	  def unpickle(s: String) = {
	    val tokens = Unpickler.tokenize(s)
	    Some((unpickler1.unpickle(tokens.head).get, unpickler2.unpickle(tokens.last).get))
	  }
	}
	
	implicit def arrayUnpickler[T](implicit m: ClassManifest[T], elementUnpickler: Unpickler[T]): Unpickler[Array[T]] = new Unpickler[Array[T]] {
	  def unpickle(s: String) = {
	    val tokens = Unpickler.tokenize(s)
	    Some(tokens.flatMap(t => elementUnpickler.unpickle(t)).toArray)
	  }
	}
	
}

object DefaultUnpicklers extends DefaultUnpicklers