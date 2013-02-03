/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.util

object Timing {
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
