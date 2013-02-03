/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.landscape

import java.awt.geom.Point2D

object PolygonUtilities {

  def getArea(polyPoints: Array[_ <: Point2D]): Double = {
    val n = polyPoints.size

    var sum = 0d
    (0 until n).foreach { i =>
      val j = (i + 1) % n
      val x = polyPoints(i).getX * polyPoints(j).getY - polyPoints(j).getX * polyPoints(i).getY
      sum += x
    }
    sum / 2.0d
    //		val area = (0 until n).flatMap(i => {
    //			val j = (i + 1) % n
    //			List(polyPoints(i).getX * polyPoints(j).getY, -1 * polyPoints(j).getX * polyPoints(i).getY)
    //		}).reduceLeft(_ + _) / 2.0d
    //		return area
  }

  def centerOfMass(polyPoints: Array[_ <: Point2D]): Point2D = {
    val area = 6 * getArea(polyPoints)
    val n = polyPoints.size

    val ret = new Point2D.Double
    (0 until n).foreach{i =>
      val j = (i + 1) % n
      val factor = polyPoints(i).getX * polyPoints(j).getY - polyPoints(j).getX * polyPoints(i).getY
      val a = (polyPoints(i).getX + polyPoints(j).getX) * factor
      val b = (polyPoints(i).getY + polyPoints(j).getY) * factor
      ret.x += a
      ret.y += b
    }
    val factor = 1.0 / area
    ret.x *= factor
    ret.y *= factor
    
    return ret
    
//    val (cx, cy) = (0 until n).map(i => {
//      val j = (i + 1) % n
//      val factor = polyPoints(i).getX * polyPoints(j).getY - polyPoints(j).getX * polyPoints(i).getY
//      val a = (polyPoints(i).getX + polyPoints(j).getX) * factor
//      val b = (polyPoints(i).getY + polyPoints(j).getY) * factor
//      (a, b)
//    }).foldLeft((0d, 0d))((s1, s2) => (s1._1 + s2._1, s1._2 + s2._2))
//
//    val factor = 1.0 / area
//
//    val ret = new Point2D.Double(cx * factor, cy * factor)
//    return ret
  }

//  	def centerOfMassOld(polyPoints: Array[_ <: Point2D]): Point2D = {
//  		val area = 6 * getArea(polyPoints)
//  		val n = polyPoints.size
//  		
//  		val (cx, cy) = (0 until n).map(i => {
//  			val j = (i + 1) % n
//  			val factor = polyPoints(i).getX*polyPoints(j).getY - polyPoints(j).getX*polyPoints(i).getY
//  			val a = (polyPoints(i).getX + polyPoints(j).getX) * factor
//  			val b = (polyPoints(i).getY + polyPoints(j).getY) * factor
//  			(a, b)
//  		}).foldLeft((0d, 0d))((s1, s2) => (s1._1 + s2._1, s1._2 + s2._2))
//  		
//  		val factor = 1.0 / area
//  		
//  		val ret = new Point2D.Double(cx * factor, cy * factor)
//  		return ret
//  	}
}
