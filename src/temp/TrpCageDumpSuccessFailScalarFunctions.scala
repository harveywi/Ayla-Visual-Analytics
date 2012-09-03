package temp

import java.io._
import edu.osu.compgeom.util.IO._

object TrpCageDumpSuccessFailScalarFunctions {

  def main(args: Array[String]): Unit = {
    val outDirSuccess = new File("/home/harveywi/research/ScalaCrystals/TrpCage/scalar_functions/success")
    val outDirFail = new File("/home/harveywi/research/ScalaCrystals/TrpCage/scalar_functions/fail")
    List(outDirSuccess, outDirFail).filterNot(_.exists()).foreach(_.mkdirs())

    val (success, fail) = getSuccessFail

    val pdbFileNames = withBufferedReader(new File("/home/harveywi/research/ScalaCrystals/TrpCage/conformation_filenames.txt")) { br =>
      val dropString = "/media/intel_ssd/TrpCage/"
      val start = dropString.length
      Iterator.continually(br.readLine).takeWhile(_ != null).map { line =>
        line.substring(start)
      }.toArray
    }

    success.foreach { s =>
      val outFile = new File(outDirSuccess, s.trajID + ".txt")
      withBufferedWriter(outFile) { bw =>
        bw.write(s.trajID + "\n")
        pdbFileNames.foreach { pdbFileName =>
          val x: Int = if (s.contains(pdbFileName)) 1 else 0
          bw.write(x + "\n")
        }
      }
    }

    fail.foreach { s =>
      val outFile = new File(outDirFail, s.trajID + ".txt")
      withBufferedWriter(outFile) { bw =>
        bw.write(s.trajID + "\n")
        pdbFileNames.foreach { pdbFileName =>
          val x: Int = if (s.contains(pdbFileName)) 1 else 0
          bw.write(x + "\n")
        }
      }
    }

  }

  sealed abstract class Trajectory(trajID: String, roi: Range) {
    def contains(pdbFileName: String): Boolean = {
      val trajID = pdbFileName.take(4)
      if (trajID == this.trajID) {
        val trajStep = pdbFileName.substring(5, pdbFileName.length - 4).toInt
        roi.contains(trajStep)
      } else {
        false
      }
    }
  }
  case class Success(trajID: String, roi: Range) extends Trajectory(trajID, roi)
  case class Fail(trajID: String, roi: Range) extends Trajectory(trajID, roi)

  def getSuccessFail = {
    val inFile = new File("/media/intel_ssd/traj.txt")
    val pat = "-".r
    val rois = withBufferedReader(new File("/media/intel_ssd/traj.txt")) { br =>
      Stream.continually(br.readLine).takeWhile(_ != null).map { line =>
        val lineSplit = pat.split(line)
        val trajID = lineSplit(0)
        val roi = (lineSplit(1).toInt to lineSplit(2).toInt)
        if (lineSplit(3).startsWith("r")) {
          Fail(trajID, roi)
        } else {
          Success(trajID, roi)
        }
      }.toArray
    }

    val successes = rois.collect { case x: Success => x }
    val fails = rois.collect { case x: Fail => x }
    (successes, fails)
  }

}