package edu.osu.compgeom.topology

abstract class StandardPersistencePairLike(val extremum: Int, val saddle: Int, val killedBy: Int) extends Ordered[StandardPersistencePairLike] {
	val persistence: Float
	override def compare(o: StandardPersistencePairLike) = persistence.compare(o.persistence)
}