package temp

import java.io._
object ChangeConformationLocations {

  def main(args: Array[String]): Unit = { 
    val br = new BufferedReader(new FileReader(new File("/home/harveywi/research/ScalaCrystals/TrpCage/conformation_filenames.txt")))
    val bw = new BufferedWriter(new FileWriter(new File("/home/harveywi/research/ScalaCrystals/TrpCage/conformation_zip_paths.txt")))
    
    val dropAmt = "/media/intel_ssd/".length()
    while (br.ready) {
      val line = br.readLine
      val newLine = line.drop(dropAmt)
      println(newLine)
      bw.write(newLine)
      bw.write("\n")
    }
    bw.flush
    bw.close
    
  }
  

}