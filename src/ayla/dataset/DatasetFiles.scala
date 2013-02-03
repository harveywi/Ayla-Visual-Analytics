/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.dataset

import java.io._
import scalaz.{ Validation, Success, Failure }
import scalaz.Validation._
import ayla.pdb.PdbStreamProvider
import scala.annotation.tailrec
import ayla.pdb._
import org.apache.commons.compress.archivers.zip.ZipFile

class DatasetFiles(
  val pdbProviderFiles: PdbProviderFiles,
  val scalarFunctionFiles: Array[File],
  val dsspFile: Option[File],
  val pcdFile: File) {

  //  def getPdbProvider() = {
  //    def getZipPdbProvider = for {
  //      zipFile <- files.find(_.getName == "conformations.zip")
  //      zipPathsFile <- files.find(_.getName == "conformation_zip_paths.txt")
  //    } yield for {
  //      zip <- catchException(new ZipFile(zipFile))
  //      zipPaths <- catchException(scala.io.Source.fromFile(zipPathsFile).getLines.toArray)
  //    } yield new ZipFilePdbStreamProvider(zip, zipPaths)
  //
  //    def getFilePdbProvider = for {
  //      file <- files.find(_.getName == "conformation_filenames.txt")
  //    } yield for {
  //      conformationFilenames <- catchException(scala.io.Source.fromFile(file).getLines.map(new File(_)).toArray)
  //    } yield new FilePdbStreamProvider(conformationFilenames)
  //
  //    getZipPdbProvider orElse getFilePdbProvider
  //  }
}

sealed abstract class PdbProviderFiles {
  def create: PdbStreamProvider
}

object PdbProviderFiles {
  def validate(datasetDir: File): Validation[String, PdbProviderFiles] = {
    def fileOpt(name: String) = {
      val f = new File(datasetDir, name)
      if (f.exists) Some(f) else None
    }

    def getZipPdbFiles = for {
      zipFile <- fileOpt("conformations.zip")
      zipPathsFile <- fileOpt("conformation_zip_paths.txt")
    } yield ZipPdbProviderFiles(zipFile: File, zipPathsFile: File)

    def getFilePdbFiles = for (file <- fileOpt("conformation_filenames.txt")) yield FilePdbProviderFiles(file)

    getZipPdbFiles orElse getFilePdbFiles match {
      case Some(pdbFiles) => Success(pdbFiles)
      case None =>
        val msg =
          """|Couldn't find the conformation files in the dataset directory.  Make sure that either:
          	 |1.  Conformations are contained in 'conformations.zip' and their zip paths are listed in 'conformation_zip_paths.txt', or
          	 |2.  A file called 'conformation_filenames.txt' listing the absolute paths to the conformations is in the dataset directory.""".stripMargin
        Failure(msg)
    }
  }
}

case class ZipPdbProviderFiles(zipFile: File, zipPathsFile: File) extends PdbProviderFiles {
  override def create = new ZipFilePdbStreamProvider(new ZipFile(zipFile), scala.io.Source.fromFile(zipPathsFile).getLines.toArray)
}

case class FilePdbProviderFiles(conformationFilenamesFile: File) extends PdbProviderFiles {
  override def create = new FilePdbStreamProvider(scala.io.Source.fromFile(conformationFilenamesFile).getLines.map(new File(_)).toArray)
}

//object DatasetFiles {
//  def validateFiles(datasetDir: File) = if (datasetDir.exists && datasetDir.isDirectory) {
//    import scalaz._
//    import Scalaz._
//
//    def getTextFilesRecursively(f: File): Array[File] = {
//      val (dirs, nonDirs) = f.listFiles.partition(_.isDirectory)
//      nonDirs.filter(_.getName.endsWith(".txt")) ++ dirs.flatMap(getTextFilesRecursively)
//    }
//
//    def getScalarFunctionFiles = new File(datasetDir, "scalar_functions") match {
//      case sfDir if sfDir.exists && sfDir.isDirectory => Success(getTextFilesRecursively(datasetDir))
//      case f @ _ => Failure(s"Directory not found: ${f.getAbsolutePath}")
//    }
//
//    def getPcdFile = new File(datasetDir, "pcd_v3.dat") match {
//      case f if f.exists => Success(f)
//      case f @ _ => Failure(s"File not found: ${f.getAbsolutePath}")
//    }
//
//    for {
//      scalarFunctionFiles <- getScalarFunctionFiles
//      pdb <- PdbProviderFiles.validate(datasetDir)
//      pcdFile <- getPcdFile
//    } yield new DatasetFiles(pdb,
//      scalarFunctionFiles,
//      datasetDir.listFiles.find(_.getName == "secondary_structure_dssp.dat"),
//      pcdFile)
//  } else {
//    Failure(s"The dataset directory ${datasetDir.getAbsolutePath} was not found.")
//  }
//}
