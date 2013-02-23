package ayla.pickling2

import shapeless._
import scala.reflect.ClassTag
import java.io.{DataOutputStream, DataInputStream}

object Pickling {
  // ***************** Pickling stuff *********************** //
  def pickler[T : ClassTag] = new MakePickler[T]

  class MakePickler[T : ClassTag] {
    def create[L <: HList]()(implicit iso: Iso[T, L], picklerHelper: PicklerHelper[L]): Pickler[T] =
      new IsoPickler(iso, picklerHelper)
  }

  class IsoPickler[T : ClassTag, L <: HList](iso: Iso[T, L], picklerHelper: PicklerHelper[L])
    extends Pickler[T] {
    def pickle(t: T, daos: DataOutputStream): Unit = {
//      val ret = ByteString(s"${t.getClass.getName.length} ${t.getClass.getName}") ++ picklerHelper.pickle(iso.to(t))
//      ByteString(s"${ret.length} ") ++ ret
      daos.writeUTF(t.getClass.getName)
      picklerHelper.pickle(iso.to(t), daos)
    }
  }

  trait PicklerHelper[L <: HList] {
    def pickle(values: L, daos: DataOutputStream): Unit
  }

  object PicklerHelper {
    implicit def hnilDecomposer = new PicklerHelper[HNil] {
      def pickle(values: HNil, daos: DataOutputStream): Unit = {/* Do nothing */}
    }

    implicit def hlistDecomposer1[H, T <: HList](implicit dh: Pickler[H], dt: PicklerHelper[T]) =
      new PicklerHelper[H :: T] {
        def pickle(values: H :: T, daos: DataOutputStream): Unit = {
          dh.pickle(values.head, daos)
          dt.pickle(values.tail, daos)
          //          dh.pickle(values.head) ++ dt.pickle(values.tail)
        }

      }
  }

  // ***************** Unpickling stuff *********************** //
  def unpickler[T: ClassTag] = new MakeUnpickler[T]
  class MakeUnpickler[T: ClassTag] {
    def create[L <: HList]()(implicit iso: Iso[T, L], unpicklerHelper: UnpicklerHelper[L]): Unpickler[T] =
      new IsoUnpickler(iso, unpicklerHelper)
  }

  class IsoUnpickler[T: ClassTag, L <: HList](iso: Iso[T, L], unpicklerHelper: UnpicklerHelper[L])
    extends Unpickler[T] {
    def unpickle(dais: DataInputStream) = {
      // Read and throw away the class name -- it's only needed for bootstrapping the unpickling process
      val className = dais.readUTF
      iso.from(unpicklerHelper.unpickleHList(dais))
    }
//    def unpickle(sourceIn: String) = {
//      val classTag = implicitly[ClassTag[T]]
//      val source = if (sourceIn.takeWhile(_ != ' ').toInt == sourceIn.substring(sourceIn.indexOf(' ') + 1).length)
//        sourceIn.drop(sourceIn.indexOf(' ') + 1)
//      else
//        sourceIn
//      
//      val tokens = Unpickler.tokenize(source)
//      val pickleClassName = tokens.head
//      if (classTag.runtimeClass.getName == pickleClassName) {
//        for {
//          l <- unpicklerHelper.unpickleTokens(tokens.tail)
//        } yield iso.from(l)
//      } else {
//        None
//      }
//    }
  }

  trait UnpicklerHelper[L <: HList] {
    def unpickleHList(dais: DataInputStream): L
  }

  object UnpicklerHelper {
    implicit val hnilExtractor = new UnpicklerHelper[HNil] {
      def unpickleHList(dais: DataInputStream) = HNil
    }

    implicit def hlistExtractor1[H, T <: HList](implicit eh: Unpickler[H], et: UnpicklerHelper[T]) =
      new UnpicklerHelper[H :: T] {
        def unpickleHList(dais: DataInputStream) = {
          val h = eh.unpickle(dais)
          val t = et.unpickleHList(dais)
          h :: t
//          for {
//            h <- eh.unpickle(tokens.head)
//            t <- et.unpickleTokens(tokens.tail)
//          } yield h :: t
        }
      }
  }
  
  
  /// ***************** Combined stuff *********************** //
  def picklerUnpickler[T : ClassTag] = new MakePicklerUnpickler[T]
  
  class MakePicklerUnpickler[T : ClassTag] {
    def create[L <: HList]()
      (implicit iso: Iso[T, L], picklerHelper: PicklerHelper[L], unpicklerHelper: UnpicklerHelper[L]): (Pickler[T], Unpickler[T]) =
        (new IsoPickler(iso, picklerHelper), new IsoUnpickler(iso, unpicklerHelper))
  }
  
}