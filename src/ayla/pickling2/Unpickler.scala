package ayla.pickling2

import scala.annotation.tailrec

trait Unpickler[T] {
  def unpickle(s: String): Option[T]
}

object Unpickler {
  @tailrec
  final def tokenize(s: String, start: Int = 0, tokens: List[String] = List.empty[String]): List[String] = {
//    println("Tokenizing " + s)
//      println("Starting search at " + s.substring(start))
      val idxSpace = s.indexOf(' ', start)
    if (start == s.length || idxSpace == -1) {
      tokens.reverse
    } else {
      val dataLengthStr = s.substring(start, idxSpace)
      val dataLength = dataLengthStr.toInt
      val ofs = dataLengthStr.length + 1
      val substring = s.substring(start + ofs, start + ofs + dataLength)
      tokenize(s, start + ofs + dataLength, substring :: tokens)
    }
  }
}