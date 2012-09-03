package edu.osu.compgeom.ayla

import scala.tools.nsc.interpreter.ILoop

object AylaREPL {
  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      println("Args required:  dataset_home_dir")
      System.exit(0)
    }

    lazy val urls = java.lang.Thread.currentThread.getContextClassLoader match {
      case cl: java.net.URLClassLoader => cl.getURLs.toList
      case _ => sys.error("classloader is not a URLClassLoader")
    }
    lazy val classpath = urls map { _.toString }
    
    val iloop = new ILoop
    val settings = new scala.tools.nsc.Settings
    settings.classpath.value = classpath.distinct.mkString(java.io.File.pathSeparator)
    iloop.process(settings)
  }
}