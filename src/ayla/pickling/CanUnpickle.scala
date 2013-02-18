package ayla.pickling

import shapeless._
import scala.annotation.tailrec
import scala.reflect.runtime.universe._

object CanUnpickle {
  def parse[T](op: String => T) = (s: String) => op(s)
}

class CanUnpickle[ParserType <: HList](parser: ParserType) {
  PickleRegistry.register(this)
  type CaseClass

  type M = scala.collection.mutable.ArrayBuffer[String]
  object mapToRemoveArrayBuffer extends Poly1 {
    implicit def casef1[T] = at[Function1[String, T]](f => (m: M) => f(m.remove(0)))
  }

  @tailrec
  private[this] def tokenize(s: String, start: Int = 0, tokens: List[String] = List.empty[String]): List[String] = {
    if (start == s.length) {
      tokens.reverse
    } else {
      val idxSpace = s.indexOf(' ', start)
      val dataLengthStr = s.substring(start, idxSpace)
      val dataLength = dataLengthStr.toInt
      val ofs = dataLengthStr.length + 1
      val substring = s.substring(start + ofs, start + ofs + dataLength)
      tokenize(s, start + ofs + dataLength, substring :: tokens)
    }
  }
  
  def unpickle[H1 <: HList, H2 <: HList, H3 <: HList, H4 <: HList](pickle: String)(
    implicit mapper: MapperAux[mapToRemoveArrayBuffer.type, ParserType, H2],
    cm: ConstMapperAux[M, H2, H3],
    zipper: ZipApplyAux[H2, H3, H4],
    iso: Iso[CaseClass, H4]): Option[CaseClass] = {
    val tokens = tokenize(pickle)
    val m = new scala.collection.mutable.ArrayBuffer[String]
    m ++= tokens

    val myName = this.getClass.getName.dropRight(1)
    if (myName.compareTo(m.remove(0)) != 0) {
      None: Option[CaseClass]
    } else {
      object foo extends Poly1 {
        implicit def caseS[T] = at[Function1[String, T]](f => f(m.remove(0)))
      }

      val p = parser.map(mapToRemoveArrayBuffer)
      val ms = p.mapConst(m)
      val z = zipper(p, ms)
      Some(iso.from(z)): Option[CaseClass]
    }
  }
}