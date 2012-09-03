package edu.osu.compgeom.scripts

import java.io._
import scala.sys.process._
import edu.osu.compgeom.util.IO._

object TrpCageEnergies {
  
  def main(args: Array[String]): Unit = {
  	findMismatchingDirs
  }
  
  def findMismatchingDirs = {
    println("Start")
    val dir = new File("/home/harveywi/research/summary_trpcage")
    val map = new scala.collection.mutable.HashMap[Int, Int]
    dir.listFiles.filter(_.getName.endsWith(".SUMMARY")).foreach(summaryFile => {
        val numLines = withBufferedReader(summaryFile){br => Iterator.continually(br.readLine).takeWhile(_ != null).size}
        val trajID = summaryFile.getName.split('_')(1).toInt
        map(trajID) = numLines - 1
      })
      
    withBufferedReader(new File(dir, "conformation_filenames.txt")){br =>
      Iterator.continually(br.readLine).takeWhile(_ != null).foreach(line => {
        val idx = parseTrajIdx(line)
        map(idx._1) = map(idx._1) - 1
      })
    }
    
    map.filter(_._2 != 0).foreach(item => {
      println(item)
    })
    
    val t = 3
  }
  
  def parseTrajIdx(name: String) = {
    val idx1 = name.lastIndexOf('/')
    val idx2 = name.lastIndexOf('_')
    val idx3 = name.lastIndexOf('.')
    val id1 = name.substring(idx1+1, idx2).toInt
    val id2 = name.substring(idx2+1, idx3).toInt
    (id1, id2)
  }
  
  def getConformationFilenames = {
    val dir = new File("/media/My Passport/hong/extracted")
    withBufferedWriter(new File("/home/harveywi/research/summary_trpcage/conformation_filenames.txt")){bw =>
      val pdbFiles = dir.listFiles.filter(pdbFile => {
        val wcProcess = Process("wc -l " + pdbFile.getName, pdbFile.getParentFile())
        val numLines = wcProcess.lines.head.takeWhile(_ != ' ').toInt
        println("File " + pdbFile + " has lines " + numLines)
        numLines == 307
      }).sortWith((f1, f2) => {
        val idx1 = parseTrajIdx(f1.getName)
        val idx2 = parseTrajIdx(f2.getName)
        val c = idx1._1.compare(idx2._1)
        if (c != 0) {
          c < 0
        } else {
          idx1._2 < idx2._2
        }
      })
      println(pdbFiles.size)
      
      pdbFiles.foreach(pdbFile => {
        bw.write(pdbFile.getAbsolutePath + "\n")
      })
    }
  }
  
  
  def parsePotentialEnergies = {
    val dir = new File("/home/harveywi/research/summary_trpcage")
    val allPotentialEnergies = dir.listFiles.filter(_.getName.endsWith(".SUMMARY")).sortBy(_.getName()).flatMap(summaryFile => {
      println("Parsing summary file " + summaryFile.getName)
      val pat = "\\s+".r
      val a = withBufferedReader(summaryFile) { br =>
        Iterator.continually(br.readLine).takeWhile(_ != null).drop(1).map(line => {
          val lineSplit = pat.split(line)
          lineSplit(8).toFloat
        })
      }
      println(a.size)
      a
    })
    
    println(allPotentialEnergies.size)

    withBufferedWriter(new File(dir, "potential_energy.txt")) { bw =>
      bw.write("Potential Energy\n")
      allPotentialEnergies.foreach(e => bw.write(e + "\n"))
    }    
  }
}