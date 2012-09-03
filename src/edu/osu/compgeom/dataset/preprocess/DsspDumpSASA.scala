package edu.osu.compgeom.dataset.preprocess

import edu.osu.compgeom.dataset._
import edu.osu.compgeom.util.IO._
import java.io._
import scopt.OptionParser
import scala.sys.process._
import scala.actors.Futures._
import scala.actors.Actor._

object DsspDumpSASA {

  class CommandLineConfig {
    var datasetDir: File = null
  }

  def main(args: Array[String]): Unit = {
    val config = new CommandLineConfig
    val dsspCommand = "/home/harveywi/Desktop/dssp-2-linux-amd64"
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
      val outDir = new File(config.datasetDir, "scalar_functions/SASA")
      if (!outDir.exists) {
        outDir.mkdirs()
      }

      val conformationFilenamesFile = new File(config.datasetDir, "conformation_filenames.txt")

      val numConformations = withBufferedReader(conformationFilenamesFile) { br =>
        Iterator.continually(br.readLine()).takeWhile(_ != null).size
      }

      val numResidues = {
        withBufferedReader(conformationFilenamesFile) { br =>
          // Get last line of some PDB file in the dataset, and grab the residue ID of it.
          // That will equal the number of residues in the protein
          val somePdbFile = new File(Iterator.continually(br.readLine).next())
          val lastPdbLine = withBufferedReader(somePdbFile) { br => Stream.continually(br.readLine).takeWhile(_ != null).filter(_.startsWith("ATOM")).last }
          val lastResidueID = lastPdbLine.substring(22, 27).trim.toInt
          lastResidueID
        }
      }
      println("Num residues:  " + numResidues)

      // 34 to 37, inclusive
      val sasaInfo = new Array[Int](numConformations * numResidues)

      final class DsspOutputParser(conformationID: Int) {
        var numLinesRead = 0
        var residueIdx = 0
        @inline
        def processLine(line: String): Unit = {
          numLinesRead += 1
          if (numLinesRead > 25) {
            val ofs = conformationID * numResidues + residueIdx
            val acc = line.substring(34, 38).trim.toInt
            sasaInfo(ofs) = acc
            residueIdx += 1
          }
        }
      }

      val tasks = withBufferedReader(new File(config.datasetDir, "conformation_filenames.txt"))(br => {
        Iterator.continually(br.readLine()).takeWhile(_ != null).toList.zipWithIndex.map {
          case (conformationFilename, i) => {
            future {
              println((i + 1) + " of " + numConformations)

              val pb = Process(dsspCommand + " " + conformationFilename)
              val outputParser = new DsspOutputParser(i)
              val procLog = ProcessLogger(outputParser.processLine(_))
              pb.!(ProcessLogger(outputParser.processLine(_)))

            }
          }
        }
      })

      tasks.grouped(32).foreach { group => scala.actors.Futures.awaitAll(Long.MaxValue / 2L, group: _*) }

      (0 until numResidues).foreach { residueID =>
        val outFile = new File(outDir, (residueID+1) + ".txt")
        withBufferedWriter(outFile) { bw =>
          bw.write("SASA " + (residueID+1) + "\n")
          (0 until numConformations).foreach { conformationID =>
            val ofs = conformationID * numResidues + residueID
            bw.write(sasaInfo(ofs) + "\n")
          }
        }
      }

      val sasaSumOutFile = new File(config.datasetDir, "scalar_functions/sasa_sum.txt")
      withBufferedWriter(sasaSumOutFile) { bw =>
        bw.write("SASA sum\n")
        (0 until numConformations).foreach { conformationID =>
          val sum = (0 until numResidues).map { residueID =>
            val ofs = conformationID * numResidues + residueID
            sasaInfo(ofs)
          }.sum
          bw.write(sum + "\n")
        }
      }

      val oos = new ObjectOutputStream(new FileOutputStream(new File(config.datasetDir, "sasa_allresidues_save.dat")))
      oos.writeObject(sasaInfo)
      oos.flush
      oos.close

      println("Done!")

    } else {
      parser.showUsage
      System.exit(0)
    }

  }

}