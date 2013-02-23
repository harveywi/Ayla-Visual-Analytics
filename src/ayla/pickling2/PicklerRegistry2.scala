package ayla.pickling2

import scala.collection._
import ayla.protocol._
import scala.reflect.runtime._
import java.io.DataInputStream

object PicklerRegistry2 {
	private[this] val picklers = new mutable.ArrayBuffer[Pickler[_]] with mutable.SynchronizedBuffer[Pickler[_]]
	private[this] val unpicklers = new mutable.ArrayBuffer[Unpickler[_]] with mutable.SynchronizedBuffer[Unpickler[_]]
	
	def register(pair: (Pickler[_], Unpickler[_])): Unit = {
	  picklers += pair._1
	  unpicklers += pair._2
	}
	
	private[this] val mirror = universe.runtimeMirror(getClass.getClassLoader)
	def unpickle(className: String, dais: DataInputStream): Any = {
	  val c = Class.forName(className + "$")
	  val companion = c.getField("MODULE$").get(c).asInstanceOf[{val unpickler: Unpickler[_]}]
	  companion.unpickler.unpickle(dais)
	}
//	def unpickle(pickle: String): Option[Any] = {
//	  val className = {
//	    val idxAfterFirstSpace = pickle.indexOf(' ') + 1
//	    val idxSecondSpace = pickle.indexOf(' ', idxAfterFirstSpace)
//	    val lenStr = pickle.substring(idxAfterFirstSpace, idxSecondSpace)
//	    val len = lenStr.toInt
//	    pickle.substring(idxSecondSpace + 1, idxSecondSpace + 1 + len)
//	  }
//	  
//	  val c = Class.forName(className + "$")
//	  
//	  val companion = c.getField("MODULE$").get(c).asInstanceOf[{val unpickler: Unpickler[_]}]
//	  companion.unpickler.unpickle(pickle)
//	}
	
}