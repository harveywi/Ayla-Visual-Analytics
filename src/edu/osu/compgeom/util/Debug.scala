package edu.osu.compgeom.util

object Debug {
	var debugMode = false
	
	def apply[T](task: String)(f: => T):Option[T] = {
		if (debugMode) {
			println("Performing task: " + task)
			val t1 = System.currentTimeMillis
			val result = f
			val t2 = System.currentTimeMillis
			val seconds = (t2 - t1) / 1000.0
			println("DEBUG Time(" + task + "): " + seconds + " seconds.")
			Some(result)
		} else None
	}
}