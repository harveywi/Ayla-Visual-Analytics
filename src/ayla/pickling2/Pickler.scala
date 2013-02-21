package ayla.pickling2

import shapeless._
import scala.reflect.ClassTag

abstract class Pickler[T : ClassTag] {
//  implicit def classTag: ClassTag[T]
	def pickle(t: T): String
}