package edu.osu.compgeom.util

object Timing {
//  def time[T, R](f: => T, res:(T, Long) => R):R = {
//    val startTime = System.currentTimeMillis
//    res(f, System.currentTimeMillis - startTime)
//  }
	
	def apply[R](task: String)(f: => R): R = {
		println("Performing task: " + task)
		val t1 = System.currentTimeMillis
		val result = f
		val t2 = System.currentTimeMillis
		val seconds = (t2 - t1) / 1000.0
		println("Time(" + task + "): " + seconds + " seconds.")
		return result
	}
}