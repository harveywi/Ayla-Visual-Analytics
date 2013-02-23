package ayla.pickling2

import shapeless._
import scala.reflect.ClassTag
import java.io.DataOutputStream
//import akka.util.ByteString

abstract class Pickler[T : ClassTag] {
//  implicit def classTag: ClassTag[T]
	def pickle(t: T, daos: DataOutputStream): Unit
}