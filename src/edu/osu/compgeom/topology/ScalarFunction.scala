package edu.osu.compgeom.topology

import java.util.Comparator
import com.google.common.collect.HashMultimap
import scala.collection.JavaConversions._
import edu.osu.compgeom.util.UnionFind

@SerialVersionUID(1L)
class ScalarFunction(vertices: Array[Array[Float]], faces: Array[Array[Int]], val getFuncVal: (Int => Float)) extends SimplicialComplex(vertices, faces) 
with Serializable {
	
	val minFuncVal = vertices.indices.map(getFuncVal(_)).min
	val maxFuncVal = vertices.indices.map(getFuncVal(_)).max
	val rangeFuncVal = if (maxFuncVal - minFuncVal != 0) maxFuncVal - minFuncVal else 1

	@SerialVersionUID(1L)
	class VertComparator extends java.util.Comparator[Int] with Serializable {
	  @inline
		override def compare(v1: Int, v2: Int): Int = {
			val f1 = getFuncVal(v1)
			val f2 = getFuncVal(v2)
			
			if (f1 < f2) {
			  return -1
			} else if (f1 > f2) {
			  return 1
			} else if (v1 < v2) {
			  return -1
			} else if (v1 > v2) {
			  return 1
			} else {
			  return 0
			}
		}	  
	}
	
	val vc = new VertComparator
	
	def this(domain: SimplicialComplex, getFuncVal: Int => Float) = this(domain.vertices, domain.faces, getFuncVal)
	
	def getGradientMag(v1: Int, v2: Int): Double = {
		val d = dist(v1, v2)
		if (d == 0) {
			return 0
		}
		return math.abs(getFuncVal(v1) - getFuncVal(v2)) / d
	}
	
	def dist(v1: Int, v2: Int): Double = {
		// This should be faster than the cleaner version below
		var t = 0d
		vertices(v1).indices.foreach(i => {
			val diff = vertices(v1)(i) - vertices(v2)(i)
			t += (diff*diff)
		})
		//val t = (vertices(v1), vertices(v2)).zipped.map{case (a, b) => (a-b)*(a-b)}.reduceLeft(_+_)
//		val t = vertices(v1).indices.map{i =>
//			val diff = vertices(v1)(i) - vertices(v2)(i)
//			diff*diff
//		}.sum
		return math.sqrt(t)
	}
	
	lazy val getSteepestUpNeighborOf: Array[Int] = Array.range(0, vertices.size).map{v =>
		getLargerNeighborsOf(v).foldLeft(v)((v1, v2) => if (getGradientMag(v, v1) > getGradientMag(v, v2)) v1 else v2)
	}
	
	lazy val getSteepestDownNeighborOf: Array[Int] = Array.range(0, vertices.size).map{v =>
		getSmallerNeighborsOf(v).foldLeft(v)((v1, v2) => if (getGradientMag(v, v1) > getGradientMag(v, v2)) v1 else v2)
	}
	
	def getLargerNeighborsOf(v: Int): Iterator[Int] = getNeighbors(v).iterator.filter(n => vc.compare(v, n) < 0)
	def getSmallerNeighborsOf(v: Int): Iterator[Int] = getNeighbors(v).iterator.filter(n => vc.compare(v, n) > 0)
	
//	def getLargerNeighborsOf(v: Int): Iterable[Int] = {
//		getNeighbors(v).view.filter(n => vc.compare(v, n) < 0)
//	}
//	
//	def getSmallerNeighborsOf(v: Int): Iterable[Int] = {
//		getNeighbors(v).view.filter(n => vc.compare(v, n) > 0)
//	}
	
	class StandardPersistencePair(extremum: Int, saddle: Int, killedBy: Int) extends StandardPersistencePairLike(extremum, saddle, killedBy) {
		override val persistence = math.abs(getFuncVal(extremum) - getFuncVal(saddle))
	}
	
	def getStandardPersistencePairs(): Array[StandardPersistencePair] = {
		val n = vertices.size
		val filtration = (0 until n).toArray.sortWith(vc.compare(_, _) < 0)
		val pairsSweepUp = sweep(filtration, getSmallerNeighborsOf, vc.compare(_, _))
		val pairsSweepDown = sweep(filtration.reverse, getLargerNeighborsOf, -vc.compare(_, _))
		
		return (pairsSweepUp ::: pairsSweepDown).toArray
	}
	
	private def sweep(vertFiltration: Array[Int], link: Int => Iterator[Int], comp: (Int, Int) => Int): List[StandardPersistencePair] = {
		val n = vertices.size
		val uf = new UnionFind(n)//new HashUnionFind((0 until n).toSet)
		val componentRep = new scala.collection.mutable.HashMap[Int, Int]
		
		val mostExtreme = vertFiltration.reduceLeft((x, y) => if (comp(x, y) < 0) x else y)
		println("Global:  " + mostExtreme)
		
		val extrema = new scala.collection.mutable.HashSet[Int]
		val pairs = new scala.collection.mutable.ListBuffer[StandardPersistencePair]
		vertFiltration.foreach {n1 =>
			val neighbors = link(n1).toList
			neighbors.size match {
				case 0 => {
					componentRep(n1) = n1
					extrema += n1
				}
				case 1 => {
					val n2 = neighbors.head
					val rep = componentRep(uf.find(n2))
					uf.union(n1, n2)
					componentRep(uf.find(n1)) = rep
				}
				case _ => {
					for ((n2, n3) <- neighbors.zip(neighbors.drop(1))) {
						val p2 = uf.find(n2)
						val p3 = uf.find(n3)
						if (p2 != p3) {
							// Two components merge.  Output a persistence pair
							val c = comp(componentRep(p2), componentRep(p3))
							if (c < 0) {
								pairs += new StandardPersistencePair(componentRep(p3), n1, componentRep(p2))
								uf.union(n1, n2)
								uf.union(n1, n3)
								componentRep(uf.find(n1)) = componentRep(p2)
							} else if (c > 0) {
								pairs += new StandardPersistencePair(componentRep(p2), n1, componentRep(p3))
								uf.union(n1, n2)
								uf.union(n1, n3)
								componentRep(uf.find(n1)) = componentRep(p3)
							} else {
								throw new RuntimeException
							}
						} else {
							val rep = componentRep(uf.find(n2))
							uf.union(n1, n2)
							uf.union(n1, n3)
							componentRep(uf.find(n1)) = rep
						}
					}
				}
				//case n => {throw new RuntimeException("Too many lower neighbors during merge: " + n)}
			}
		}
		
		println("numExtrema:" + extrema.size)
		
		pairs.toList
	}
	
	def getMorseCrystals(simpThresh: Float): (Array[MorseCrystal], Map[Int, Int]) = {
		
			// First, grab the standard persistence pairs.
		val persistencePairs = getStandardPersistencePairs()
		println("Found " + persistencePairs.size + " persistence pairs!")
		//println(persistencePairs.map(pair => (pair.killedBy, pair.persistence)).mkString("", ",", "\n"))
		
//		// EXPERIMENT RESULT
//		// Compute some information pertaining to the geometry of the persistence pairs
//		// Specifically, we want to measure correlation between persistence and geometric distance
////		import java.io._
////		val bw = new BufferedWriter(new FileWriter(new File("/Users/harvey27/relation.csv")))
////		for (pair <- persistencePairs) {
////			val d = dist(pair.killedBy, pair.extremum)
////			val p = pair.persistence
////			bw.write(List(p, d).mkString("", ",", "\n"))
////		}
////		bw.flush
////		bw.close
//		
//		// Let's grab some statistics on the number of local minima
//		import java.io._
//		val bw = new BufferedWriter(new FileWriter(new File("/Users/harvey27/minima.csv")))
//		for (pair <- persistencePairs if getSmallerNeighborsOf(pair.extremum).size == 0) {
//			bw.write(List(getFuncVal(pair.extremum), pair.persistence).mkString("", ",", "\n"))
//		}
//		bw.flush
//		bw.close
//		
//		System.exit(0)
//		
//		// END EXPERIMENT RESULT
		
		
		val trueExtremumMap = persistencePairs.filter(_.persistence < simpThresh).map(pair => (pair.extremum -> pair.killedBy)).toMap
		/*val trueExtremumMap = persistencePairs.map(pair => {
			if (pair.persistence < simpThresh) {
				(pair.extremum -> pair.killedBy)
			} else {
				(pair.extremum -> pair.extremum)
			}
		}).toMap*/
		
		val n = vertices.size
		
		val ufUp = new UnionFind(n)
		val ufDown = new UnionFind(n)
		
		val highest = (0 until n).toArray
		val lowest = (0 until n).toArray
		
		for (i <- 0 until vertices.size) {
			// Get the largest and smallest neighbors of this point
			val neighbors = getNeighbors(i)
			val idxMax = getSteepestUpNeighborOf(i)
			val idxMin = getSteepestDownNeighborOf(i)
			//val idxMax = neighbors.foldLeft(i)((a, b) => if (vc.compare(a, b) < 0) b else a)
			//val idxMin = neighbors.foldLeft(i)((a, b) => if (vc.compare(a, b) < 0) a else b)
			
			// Update the highest/lowest crystal elements
			val oldHigh1 = highest(ufUp.find(i))
			val oldHigh2 = highest(ufUp.find(idxMax))
			ufUp.union(i, idxMax)
			highest(ufUp.find(idxMax)) = List(oldHigh1, oldHigh2, idxMax).reduceLeft((a, b) => if (vc.compare(a, b) < 0) b else a)
			
			val oldLow1 = lowest(ufDown.find(i))
			val oldLow2 = lowest(ufDown.find(idxMin))
			ufDown.union(i, idxMin)
			lowest(ufDown.find(idxMin)) = List(oldLow1, oldLow2, idxMin).reduceLeft((a, b) => if (vc.compare(a, b) < 0) a else b)
		}
		
		for ((extremum, killedBy) <- trueExtremumMap) {
			if (vc.compare(extremum, killedBy) < 0) {
				// extremum is a maximum
				ufUp.union(extremum, killedBy)
				//highest(ufUp.find(extremum)) = 
			} else {
				// extremum is a minimum
				ufDown.union(extremum, killedBy)
			}
		}
		
		val crystalMultimap = HashMultimap.create[(Int, Int), Int]
		for (i <- 0 until vertices.size) {
			val min = lowest(ufDown.find(i))
			val max = highest(ufUp.find(i))
			
			crystalMultimap.put((min, max), i)
		}
		println("Number of Morse crystals:  " + crystalMultimap.keySet.size)
		
		val keys = crystalMultimap.keySet.toSeq

		val vertexToCrystalID = new Array[Int](vertices.size)
		keys.zipWithIndex.foreach{case ((min, max), crystalID) => {
			val crystalPts = crystalMultimap.get((min, max))
			crystalPts.foreach(i => vertexToCrystalID(i) = crystalID)
		}}
		
		val crystalFaces = HashMultimap.create[Int, (Int, Int)]
		faces.foreach(f => {
			if (f.size == 2 && f.map(vertexToCrystalID).distinct.size == 1) {
				val crystalID = vertexToCrystalID(f(0))
				crystalFaces.put(crystalID, (f(0), f(1)))
			}
		})
		
		val crystals = for (((min, max), crystalID) <- keys.zipWithIndex) yield {
			val crystalPts = crystalMultimap.get((min, max))
//			val crystalEdges = for (e <- faces if e.size == 2 && e.forall(crystalPts.contains)) yield (e(0), e(1))
			val crystalEdges = crystalFaces.get(crystalID).toSeq.toArray
//			val crystalEdges = Array.empty[(Int, Int)]
			new MorseCrystal(min, max, crystalPts.map(_.toInt).toArray, crystalEdges, this)
		}
		println("Done computing crystals")
		
		return (crystals.toArray, trueExtremumMap)
	}
}

object ScalarFunction {
	
}