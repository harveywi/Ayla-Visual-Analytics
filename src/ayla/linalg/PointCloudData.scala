/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.linalg

import java.io._

object PointCloudData {
  def load(inputFile: File): Array[Array[Float]] = {
    val is = new DataInputStream(new BufferedInputStream(new FileInputStream(inputFile)))
    val n = is.readInt
    val d = is.readInt

    val pcd = Array.ofDim[Float](n, d)
    
    (0 until n).foreach { i =>
    	(0 until d).foreach {j =>
    		pcd(i)(j) = is.readFloat
    	}
    }
    
    pcd
  }

  /**
   * Format:
   * 1.  Number of points (4 bytes)
   * 2.  Number of dimensions (4 bytes)
   * 3.  Each point's coordinates (4 bytes per coordinate)
   */
  def save(pcd: Array[Array[Float]], outputFile: File): Unit = {
    val os = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(outputFile)))
    os.writeInt(pcd.length)
    os.writeInt(pcd(0).length)
    pcd.foreach(_.foreach(f => os.writeFloat(f)))
    os.flush
    os.close
  }
}
