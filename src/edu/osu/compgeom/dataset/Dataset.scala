package edu.osu.compgeom.dataset

import java.io.{File, InputStream}
import org.jgrapht.graph.{SimpleDirectedGraph, DefaultEdge}
import edu.osu.compgeom.ayla.collab.ConformationAnnotation
import scala.util.matching.Regex

trait Dataset {
  def getPDBLines(i: Int): Array[String]
  def getPDBInputStream(i: Int): InputStream
  val dsspOutput: Option[Array[Char]]
  val colorFunctions: Array[(File, String)]
  def getColorFunction(file: File): Array[Float]
  def addAnnotation(annotation: ConformationAnnotation): Unit = {}
  
  def findMatchingConformations(regex: Regex): Array[(String, Int)]
  
  def getContactDensity(conformationID: Int, residues: Array[Int]): Int
}