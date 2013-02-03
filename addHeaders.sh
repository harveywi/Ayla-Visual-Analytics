#!/bin/bash
exec scala "$0" "$@"
!#

import java.io._

val header = """/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
"""

def processFile(f: File): Unit = {
  if (f.getName.endsWith(".java") || f.getName.endsWith(".scala")) {
    println(f.getAbsolutePath)
    val br = new BufferedReader(new FileReader(f))
    val firstLine = br.readLine
    val out = new StringBuilder()
    if (firstLine.startsWith("package")) {
      // Add the header
      out.append(header)
    }
    out.append(firstLine + "\n")
    while (br.ready) {
      out.append(br.readLine + "\n")
    }
    br.close

    val bw = new BufferedWriter(new FileWriter(f))
    bw.write(out.toString)
    bw.flush
    bw.close
  } else if (f.isDirectory) {
    f.listFiles.foreach(processFile)
  }
}

processFile(new File("src/"))
