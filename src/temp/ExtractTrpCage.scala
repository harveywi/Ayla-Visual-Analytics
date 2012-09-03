package temp

import java.io._

object ExtractTrpCage {

  def main(args: Array[String]): Unit = {
    val dir = new File("/media/My Passport/hong/")
    val outDir = new File(dir, "extracted")
    if (!outDir.exists)
      outDir.mkdir()
    val pdbFiles = dir.listFiles.filter(_.getName.endsWith(".pdb"))
    pdbFiles.foreach(pdbFile => {
      println("Extracting " + pdbFile.getName)
      val br = new BufferedReader(new FileReader(pdbFile))
      var bw: BufferedWriter = null
      var idx = 0
      while (br.ready) {
        val line = br.readLine
        if (line.startsWith("MODEL")) {
          if (bw != null) {
            bw.flush
            bw.close
          }
          bw = new BufferedWriter(new FileWriter(new File(outDir, pdbFile.getName.dropRight(4) + "_" + idx + ".pdb")))
          idx += 1
        }
        bw.write(line)
        bw.write("\n")
      }
    })
  }
}