package ayla.pickling2

import scala.collection._

object PicklerRegistry2 {
	private[this] val picklers = new mutable.ArrayBuffer[Pickler[_]] with mutable.SynchronizedBuffer[Pickler[_]]
	private[this] val unpicklers = new mutable.ArrayBuffer[Unpickler[_]] with mutable.SynchronizedBuffer[Unpickler[_]]
	
	def register(pair: (Pickler[_], Unpickler[_])): Unit = {
	  picklers += pair._1
	  unpicklers += pair._2
	}
	
	def unpickle(pickle: String): Option[Any] = unpicklers.find{_.unpickle(pickle).isDefined}.flatMap(_.unpickle(pickle))

//	def pickle(x: Any): String = picklers.find{p =>
//	  
//	} 
	  
	  //unpicklers.find{_.unpickle(pickle).isDefined}.flatMap(_.unpickle(pickle))
	
}