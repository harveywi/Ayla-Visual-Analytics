package temp

import java.io._

object GenerateRandomOff {
	def main(args: Array[String]): Unit = {
	  val numVerts = 5
	  val numFaces = numVerts*(numVerts-1)*(numVerts-2) / 6
	  val bw = new BufferedWriter(new FileWriter(new File("/dev/shm/test.off")))
	  bw.write("OFF\n")
	  bw.write(numVerts + " " + numFaces + " 0\n")
	  (0 until numVerts).foreach(i => {
	    bw.write("0 " + i + " 0\n")
	  })
	  println(numFaces)
	  (0 until numVerts).combinations(3).foreach(c => {
	    bw.write(c.mkString("3 ", " ", "\n"))
	  })
	  bw.flush
	  bw.close
	}
}