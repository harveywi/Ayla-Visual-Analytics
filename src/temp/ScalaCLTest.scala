package temp

import scalacl._
import scala.math._
import edu.osu.compgeom.util.Timing

object ScalaCLTest {
	implicit val context = Context.best
  def main(args: Array[String]): Unit = {
	  val array1 = Array.fill(254250)(math.random.toFloat)
	  val array2 = array1.cl
	  
	  val norm1 = Timing("normal") {
	  	//array1.zipWithIndex.filter({ case (v, i) => (v % 2) != 0 }).map(_._1).toArray
	    math.sqrt(array1.map{x => x*x}.sum)
	  }
	  
	  println(norm1)
	  
	  val norm2 = Timing("cl") {
	  	math.sqrt(array2.map{x => x*x}.sum)
	  }
	  
	  println(norm2)
	}

}