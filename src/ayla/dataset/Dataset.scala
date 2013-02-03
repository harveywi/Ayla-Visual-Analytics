/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.dataset

import java.io.{File, InputStream}
import org.jgrapht.graph.{SimpleDirectedGraph, DefaultEdge}
import scala.util.matching.Regex
import ayla.collab.ConformationAnnotation

trait Dataset {
  def getPDBLines(i: Int): Array[String]
  def getPDBInputStream(i: Int): InputStream
  val dsspOutput: Option[Array[Char]]
  val scalarArrays: Array[File]
  def getScalarArray(file: File): Array[Float]
  def addAnnotation(annotation: ConformationAnnotation): Unit = {}
  
  def findMatchingConformations(regex: Regex): Array[(String, Int)]
  
  def getContactDensity(conformationID: Int, residues: Array[Int]): Int
}
