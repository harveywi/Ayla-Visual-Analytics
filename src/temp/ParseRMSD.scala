package temp

import java.io._

object ParseRMSD {

  def main(args: Array[String]): Unit = {
    val br = new BufferedReader(new FileReader(new File("/home/harveywi/Desktop/rmsd_raw.txt")))
    val bw = new BufferedWriter(new FileWriter(new File("/home/harveywi/Desktop/rmsd.txt")))
    bw.write("RMSD\n")
    
    val pat = "\\s+".r
    while (br.ready) {
      val line = br.readLine
      if (!line.startsWith("Final")) {
        val line2 = line.drop(5).trim
        val rmsd = pat.split(line2)(0)
        bw.write(rmsd + "\n")
      }
    }
    
    bw.flush
    bw.close
  }

}