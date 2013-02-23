/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client

import java.io.ByteArrayInputStream
import java.io.File
import java.io.InputStream
import scala.util.matching.Regex
import ayla.collab.ConformationAnnotation
import ayla.dataset.Dataset
import ayla.dataset.DsspProvider

class AylaClientCachedDataset(val client: AylaClient, val hasDsspOutput: Boolean, val scalarArrays: Array[File]) extends Dataset {
  def getPDBLines(i: Int) = client.getPDBLines(i)
  
  val dsspProvider = if (hasDsspOutput) {
    Some(new DsspProvider {
      def getDSSPLabels(numResidues: Int, id: Int): Array[Char] = client.getDSSPLabels(numResidues, id)
    })
  } else {
    None
  }
  
  def getPDBInputStream(i: Int): InputStream = {
    val pdbLines = getPDBLines(i)
    new ByteArrayInputStream(pdbLines.mkString("\n").getBytes())
  }
  
  def getScalarArray(file: File): Array[Float] = client.getColorFunction(file)
  
  override def addAnnotation(annotation: ConformationAnnotation) = client.postAnnotation(annotation)
  
  def findMatchingConformations(regex: Regex): Array[(String, Int)] = client.findMatchingConformations(regex)
  
  @deprecated("Use getContactDensities instead", "0.1")
  def getContactDensity(conformationID: Int, residues: Array[Int]): Int = client.getContactDensities(residues)(conformationID)
  
  def getContactDensities(residues: Array[Int]): Array[Int] = client.getContactDensities(residues)
  
}
