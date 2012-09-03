package edu.osu.compgeom.dataset.preprocess

import org.openscience.jmol.app.Jmol
import org.openscience.jmol.app.JmolApp
import edu.osu.compgeom.dataset._
import edu.osu.compgeom.util.IO._
import java.io._
import scopt.OptionParser
import scala.sys.process._
import scala.actors.Futures._
import scala.actors.Actor._

object DumpSecondaryStructures {

  def withBufferedReader[T](f: File)(op: BufferedReader => T): T = {
    val br = new BufferedReader(new FileReader(f))
    try {
      op(br)
    } finally {
      br.close
    }
  }

  class CommandLineConfig {
    var datasetDir: File = null
  }

  def main(args: Array[String]): Unit = {
    val config = new CommandLineConfig
    val jmolCommand = "/home/harveywi/Downloads/jmol-12.0.40/jmol.sh"
    val parser = new OptionParser("DumpSecondaryStructures") {
      arg(
        "<dataset_directory>",
        "Directory where the dataset to analyze is found.", { datasetDir: String =>
          {
            val f = new File(datasetDir)
            require(f.exists, "Error:  The specified dataset directory does not exist.  Exiting.")
            require(f.isDirectory, "Error:  dataset_directory must be a valid directory.  Exiting.")
            config.datasetDir = f
          }
        })
    }

    if (parser.parse(args)) {
      val outDir = new File(config.datasetDir, "secondary_structure")
      if (!outDir.exists)
        outDir.mkdir()

      val numConformations = withBufferedReader(new File(config.datasetDir, "conformation_filenames.txt")) { br =>
        Iterator.continually(br.readLine()).takeWhile(_ != null).size
      }

      val tasks = withBufferedReader(new File(config.datasetDir, "conformation_filenames.txt"))(br => {
        Iterator.continually(br.readLine()).takeWhile(_ != null).toList.zipWithIndex.map {
          case (conformationFilename, i) => {
            future {
              val scriptFile = new File("/dev/shm/jmol_script_" + i + ".txt")
	            val outputFile = new File(outDir, i + ".out")
	
	            println((i + 1) + " of " + numConformations)
	            val script = new StringBuilder()
	            script.append("load auto ")
	            script.append(conformationFilename)
	            script.append("\n")
	            script.append("var x = script('show DSSP')\n")
	            script.append("write var x \"%s\"\n".format(outputFile.getAbsolutePath))
	
	            withBufferedWriter(scriptFile)(_.write(script.toString))
	
	            val pb = Process(jmolCommand + " -i -n -s " + scriptFile.getAbsolutePath)
              pb.!(ProcessLogger(_ => {}))
              scriptFile.delete()
            }
          }
        }
      })
      
//      scala.actors.Futures.awaitAll(Long.MaxValue / 2L, tasks: _*)
      tasks.grouped(64).foreach{group => scala.actors.Futures.awaitAll(Long.MaxValue / 2L, group: _*)}

      println("Done.")
    }
  }
}