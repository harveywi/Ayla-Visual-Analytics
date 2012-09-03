package temp

import edu.osu.compgeom.dataset._
import edu.osu.compgeom.util.IO._
import java.io._
import scopt.OptionParser
import scala.sys.process._
import scala.actors.Futures._
import scala.actors.Actor._
import edu.osu.compgeom.util.Timing

object Rmsd {

  class CommandLineConfig {
    var datasetDir: File = null
  }

  def main(args: Array[String]): Unit = {
    val config = new CommandLineConfig
    val maxclusterCommand = (pdb1: String, pdb2: String) => "/home/harveywi/Desktop/maxcluster %s %s -rmsd".format(pdb1, pdb2)
    val parser = new OptionParser("Rmsd") {
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
//      val dataset = new CachedDataset(config.datasetDir)
      val outFile = new File(config.datasetDir, "scalar_functions/rmsd_from_nmr.txt")

      val conformationFilenamesFile = new File(config.datasetDir, "conformation_filenames.txt")

      val numConformations = withBufferedReader(conformationFilenamesFile) { br =>
        Iterator.continually(br.readLine()).takeWhile(_ != null).size
      }

      val tmpDir = new File("/dev/shm/")

      val neck1 = (2 to 5)
      val neck2 = (16 to 19)

//      def inPdbToROI(inPdbFile: File): File = {
//        val outPdbFile = new File(tmpDir, inPdbFile.getName)
//        withBufferedReader(inPdbFile) { br =>
//          withBufferedWriter(outPdbFile) { bw =>
//            var atomID = 1
//            Stream.continually(br.readLine).takeWhile(_ != null).foreach { line =>
//              if (line.startsWith("ATOM")) {
//                val seqNo = line.substring(22, 27).trim.toInt
//                if (neck1.contains(seqNo)) {
//                  val newSeqNo = seqNo - neck1.start + 1
//                  val outLine = line.substring(0, 8) + atomID.toString.reverse.padTo(3, ' ').reverse + line.substring(11, 24) + newSeqNo.toString.reverse.padTo(2, ' ').reverse + line.substring(26)
//                  bw.write(outLine + "\n")
//                  atomID += 1
//                } else if (neck2.contains(seqNo)) {
//                  val newSeqNo = seqNo - neck2.start + neck1.size + 1
//                  val outLine = line.substring(0, 8) + atomID.toString.reverse.padTo(3, ' ').reverse + line.substring(11, 24) + newSeqNo.toString.reverse.padTo(2, ' ').reverse + line.substring(26)
//                  bw.write(outLine + "\n")
//                  atomID += 1
//                }
//              } else if (line.startsWith("TER")) {
//                bw.write("TER     %3s      SER    %2s                                  \n".format(atomID.toString, (neck1.size + neck2.size)))
//              } else {
//                bw.write(line + "\n")
//              }
//            }
//          }
//        }
//        outPdbFile
//      }

//      val lines = scala.io.Source.fromFile(new File(config.datasetDir, "scalar_functions/potential_energy.txt")).getLines
//      val morseFuncVals = lines.drop(1).map(_.toFloat).toArray
//
//      val minIdx = morseFuncVals.view.zipWithIndex.sortWith((t1, t2) => t1._1 < t2._1).head._2
//
//      val minPDB_original = dataset.pdbFiles.get(minIdx)
      val minPDB_original = new File("/home/harveywi/Desktop/trpcage_native.pdb")
//      val minPdbROI = inPdbToROI(minPDB_original) 

      val rmsdVals = new Array[Float](numConformations)

      final class OutputParser(conformationID: Int) {
        @inline
        def processLine(line: String): Unit = {
          val rmsd = line.substring(5).trim.takeWhile(_ != ' ').toFloat
          rmsdVals(conformationID) = rmsd
        }
      }

      val tasks = withBufferedReader(conformationFilenamesFile)(br => {
        Iterator.continually(br.readLine()).takeWhile(_ != null).toList.zipWithIndex.map {
          case (pdbFileName, i) => {
            future {
              println((i + 1) + " of " + numConformations)
              
              val pb = Process(maxclusterCommand(minPDB_original.getAbsolutePath(), pdbFileName))
//              val roiFile = inPdbToROI(new File(pdbFileName))
//
//              val pb = Process(maxclusterCommand(minPdbROI.getAbsolutePath(), roiFile.getAbsolutePath()))
              val outputParser = new OutputParser(i)
              val procLog = ProcessLogger(outputParser.processLine(_))
              pb.!(ProcessLogger(outputParser.processLine(_)))
//              if (roiFile != minPdbROI)
//              	roiFile.delete()
            }
          }
        }
      })

      tasks.grouped(32).foreach { group => scala.actors.Futures.awaitAll(Long.MaxValue / 2L, group: _*) }

      withBufferedWriter(outFile) { bw =>
        bw.write("RMSD (From NMR)\n")
        rmsdVals.foreach(f => bw.write(f + "\n"))
      }
      println("Done!")

    } else {
      parser.showUsage
      System.exit(0)
    }

  }

}