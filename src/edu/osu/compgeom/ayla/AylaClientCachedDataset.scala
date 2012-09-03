package edu.osu.compgeom.ayla

import edu.osu.compgeom.dataset._
import java.io._
import java.util.zip.GZIPInputStream
import edu.osu.compgeom.ayla.message._
import edu.osu.compgeom.ayla.collab.ConformationAnnotation
import scala.util.matching.Regex

class AylaClientCachedDataset(val client: AylaClient) extends Dataset {
  def getPDBLines(i: Int) = client.getPDBLines(i)
  
  def getPDBInputStream(i: Int): InputStream = {
    val pdbLines = getPDBLines(i)
    new ByteArrayInputStream(pdbLines.mkString("\n").getBytes())
  }
  
  val dsspOutput = client.getDsspOutput()
  val colorFunctions = client.getColorFunctions()
  
  def getColorFunction(file: File): Array[Float] = client.getColorFunction(file)
  
  override def addAnnotation(annotation: ConformationAnnotation) = client.postAnnotation(annotation)
  
  def findMatchingConformations(regex: Regex): Array[(String, Int)] = client.findMatchingConformations(regex)
  
  @deprecated("Use getContactDensities instead", "0.1")
  def getContactDensity(conformationID: Int, residues: Array[Int]): Int = client.getContactDensities(residues)(conformationID)
  
  def getContactDensities(residues: Array[Int]): Array[Int] = client.getContactDensities(residues)
  
}