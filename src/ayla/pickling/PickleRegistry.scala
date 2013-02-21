//package ayla.pickling
//
//import scala.collection._
//
//object PickleRegistry {
//  private[this] val unpicklers = new mutable.ArrayBuffer[String => Option[Any]] with mutable.SynchronizedBuffer[String => Option[Any]]
////  private[this] val picklers = new mutable.ArrayBuffer[String => Option[Any]] with mutable.SynchronizedBuffer[String => Option[Any]]
//  def register(x: String => Option[Any]): Unit = unpicklers += x
//  
//  def unpickle(pickle: String): Option[Any] = unpicklers.find(_(pickle).isDefined).flatMap(_(pickle))
//}