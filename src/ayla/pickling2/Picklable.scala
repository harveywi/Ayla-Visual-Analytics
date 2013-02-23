package ayla.pickling2

//import shapeless._
//import scala.reflect.ClassTag
//import ayla.pickling2.Pickling._
import java.io.DataOutputStream

trait Picklable {
	def pickled(daos: DataOutputStream): Unit
}

//object Picklable {
//  def pickled[CC : ClassTag, H <: HList](c: CC)(implicit iso: Iso[CC, H], picklerHelper: PicklerHelper[H]) = pickler[CC].create().pickle(c)
//  def pickledIso[CC : ClassTag, H <: HList](implicit iso: Iso[CC, H], picklerHelper: PicklerHelper[H]) = {
//    pickler[CC].create()
//  }
//}