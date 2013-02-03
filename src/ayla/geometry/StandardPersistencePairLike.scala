/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.geometry

abstract class StandardPersistencePairLike(val extremum: Int, val saddle: Int, val killedBy: Int) extends Ordered[StandardPersistencePairLike] {
	val persistence: Float
	override def compare(o: StandardPersistencePairLike) = persistence.compare(o.persistence)
}
