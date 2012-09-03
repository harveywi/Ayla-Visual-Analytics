package edu.osu.compgeom.util

object Looping {

  @inline
  def forLoop(start: Int, endExclusive: Int, by: Int = 1)(op: Int => _) = {
    var i = start
    while (i < endExclusive) {
      op(i)
      i += by
    }
  }
  
  @inline
  def forLoopInclusive(start: Int, endInclusive: Int, by: Int = 1)(op: Int => _) = forLoop(start, endInclusive + 1, by)(op)
}