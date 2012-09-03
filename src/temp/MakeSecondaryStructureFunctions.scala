package temp

import edu.osu.compgeom.dataset._
import edu.osu.compgeom.util.IO._
import java.io._
import scopt.OptionParser
import scala.sys.process._
import scala.actors.Futures._
import scala.actors.Actor._
import edu.osu.compgeom.topology.ScalarFunction

object MakeSecondaryStructureFunctions {

  class CommandLineConfig {
    var datasetDir: File = null
  }

  def main(args: Array[String]): Unit = {
    val config = new CommandLineConfig
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
      val outDir = new File(config.datasetDir, "scalar_functions/2ary_structures")
      if (!outDir.exists())
        outDir.mkdirs()

      val conformationFilenamesFile = new File(config.datasetDir, "conformation_filenames.txt")

      val numConformations = withBufferedReader(conformationFilenamesFile) { br =>
        Iterator.continually(br.readLine()).takeWhile(_ != null).size
      }

      // Find out how many residues there are
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

      val dsspFile = new File(config.datasetDir, "secondary_structure_dssp.dat")
      val ois = new ObjectInputStream(new FileInputStream(dsspFile))
      val structureInfo = ois.readObject.asInstanceOf[Array[Char]]
      println(structureInfo.size)
      println(numConformations * numResidues)

      val ssMap = Map('X' -> "structure=0",
        'H' -> "helixalpha",
        'B' -> "sheet",
        'E' -> "sheet",
        'G' -> "helix310",
        'I' -> "helixpi",
        'T' -> "turn",
        'S' -> "bend" /* 'S' -> "bend" */ )

      def getSSInfo(conformationID: Int) = {
        val ofs = numResidues*conformationID
        Iterator.range(0, numResidues).map(i => structureInfo(ofs + i))
      }  
      
      val dataset = new CachedDataset(config.datasetDir) with FilePdbStreamProvider
      val lines = scala.io.Source.fromFile(new File(config.datasetDir, "scalar_functions/potential_energy.txt")).getLines
      val morseFuncVals = lines.drop(1).map(_.toFloat).toArray

      val vertices = dataset.pcaPoints.toArray

      println("Constructing Morse function")
      val morseFunction = new ScalarFunction(vertices, Array.empty[Array[Int]], morseFuncVals)
      
      val idxNative = morseFuncVals.indices.sortWith((i, j) => morseFunction.vc.compare(i, j) < 0).head
      //val ssNative = getSSInfo(idxNative).toArray
      val ssNative = " HHHHHHHTTGGGGT     ".toArray
        
      for ((letter, description) <- ssMap if letter != 'X') {
        
        withBufferedWriter(new File(outDir, description + ".txt")){bw =>
          bw.write(description + "\n")
          (0 until numConformations).foreach{id =>
            val count = ssNative.zip(getSSInfo(id).toSeq).count{case (x, y) => {y == letter}}
            bw.write(count + "\n")
//          	ssNative.zip(getSSInfo(id).toArray)
//          	bw.write(count + "\n")
          }
        }        
        
//        withBufferedWriter(new File(outDir, description + ".txt")){bw =>
//          bw.write(description + "\n")
//          (0 until numConformations).foreach{id =>
//          	val count = getSSInfo(id).count(_ == letter)
//          	bw.write(count + "\n")
//          }
//        }
      }

      //      val structureInfo = new Array[Char](numConformations*numResidues)
      //      
      //      final class DsspOutputParser(conformationId: Int) {
      //        var numLinesRead = 0
      //        var residueIdx = 0
      //        @inline
      //        def processLine(line: String): Unit = {
      //          numLinesRead += 1
      //          if (numLinesRead > 25) {
      //            val ofs = conformationId * numResidues + residueIdx
      //            line(16) match {
      //              case ' ' => structureInfo(ofs) = 'X'
      //              case c => structureInfo(ofs) = c
      //            }
      //            residueIdx += 1
      //          }
      //        }
      //      }
      //      
      //      val tasks = withBufferedReader(new File(config.datasetDir, "conformation_filenames.txt"))(br => {
      //        Iterator.continually(br.readLine()).takeWhile(_ != null).toList.zipWithIndex.map {
      //          case (conformationFilename, i) => {
      //            future {
      //              println((i+1) + " of " + numConformations)
      //              
      //              val pb = Process(dsspCommand + " " + conformationFilename)
      //              val outputParser = new DsspOutputParser(i)
      //              val procLog = ProcessLogger(outputParser.processLine(_))
      //              pb.!(ProcessLogger(outputParser.processLine(_)))
      //              
      //            }
      //          }
      //        }
      //      })
      //      
      //      tasks.grouped(10).foreach{group => scala.actors.Futures.awaitAll(Long.MaxValue / 2L, group: _*)}
      //      
      //      val oos = new ObjectOutputStream(new FileOutputStream(outFile))
      //      oos.writeObject(structureInfo)
      //      oos.flush()
      //      oos.close()
      //      println("Done!")

    } else {
      parser.showUsage
      System.exit(0)
    }

  }

}