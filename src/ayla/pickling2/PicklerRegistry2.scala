package ayla.pickling2

import scala.collection._
import ayla.protocol._
import scala.reflect.runtime._

object PicklerRegistry2 {
	private[this] val picklers = new mutable.ArrayBuffer[Pickler[_]] with mutable.SynchronizedBuffer[Pickler[_]]
	private[this] val unpicklers = new mutable.ArrayBuffer[Unpickler[_]] with mutable.SynchronizedBuffer[Unpickler[_]]
	
	def register(pair: (Pickler[_], Unpickler[_])): Unit = {
	  picklers += pair._1
	  unpicklers += pair._2
	}
	
//	val unpicklers2 = Seq(
//	    ClientConnectRequest.unpickler,
//ConnectToProjectRequest.unpickler,
//CreateAnnotationRequest.unpickler,
//CreateChatMessage.unpickler,
//CreateStoryboard.unpickler,
//FindMatchingConformationsRequest.unpickler,
//GetCollabProjectsRequest.unpickler,
//GetColorFunctionRequest.unpickler,
//GetContactDensitiesRequest.unpickler,
//GetContourTreeAreasRequest.unpickler,
//GetDomainShortestPathRequest.unpickler,
//GetPDBLinesRequest.unpickler,
//RefreshAnnotationsRequest.unpickler,
//RefreshChatLogRequest.unpickler,
//RefreshStoryboardsRequest.unpickler,
//
//ClientConnectResponse.unpickler,
//ConnectToProjectResponse.unpickler,
//CreateChatMessage.unpickler,
//CreateStoryboard.unpickler,
//FindMatchingConformationsResponse.unpickler,
//GetCollabProjectsResponse.unpickler,
//GetColorFunctionResponse.unpickler,
//GetContactDensitiesResponse.unpickler,
//GetContourTreeAreasResponse.unpickler,
//GetDomainShortestPathResponse.unpickler,
//GetPDBLinesResponse.unpickler,
//RefreshAnnotationsResponse.unpickler,
//RefreshChatLogResponse.unpickler,
//RefreshStoryboardsResponse.unpickler,
//ayla.server.ProjInfo.unpickler
//
//	    )

	private[this] val mirror = universe.runtimeMirror(getClass.getClassLoader)
	def unpickle(pickle: String): Option[Any] = {
	  val className = {
	    val idxAfterFirstSpace = pickle.indexOf(' ') + 1
	    val idxSecondSpace = pickle.indexOf(' ', idxAfterFirstSpace)
	    val lenStr = pickle.substring(idxAfterFirstSpace, idxSecondSpace)
	    val len = lenStr.toInt
	    pickle.substring(idxSecondSpace + 1, idxSecondSpace + 1 + len)
	  }
	  
	  val c = Class.forName(className + "$")
	  
	  val companion = c.getField("MODULE$").get(c).asInstanceOf[{val unpickler: Unpickler[_]}]
	  companion.unpickler.unpickle(pickle)
	}
	
//	def unpickle(pickle: String): Option[Any] = unpicklers2.find{u =>
//	  u.unpickle(pickle).isDefined}.flatMap(_.unpickle(pickle))
	
//	def unpickle(pickle: String): Option[Any] = unpicklers.find{u =>
//	  u.unpickle(pickle).isDefined}.flatMap(_.unpickle(pickle))

//	def pickle(x: Any): String = picklers.find{p =>
//	  
//	} 
	  
	  //unpicklers.find{_.unpickle(pickle).isDefined}.flatMap(_.unpickle(pickle))
	
}