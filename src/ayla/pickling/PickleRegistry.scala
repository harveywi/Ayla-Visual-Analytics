package ayla.pickling

object PickleRegistry {
  private[this] val unpicklers = new scala.collection.mutable.ArrayBuffer[CanUnpickle[_]]
  def register(x: CanUnpickle[_]): Unit = unpicklers += x
  
  def decodePickle(pickle: String): Any = unpicklers.find(_.unpickle(pickle).isDefined)
}