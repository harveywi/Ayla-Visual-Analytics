package edu.osu.compgeom.ayla

class HexTask[A](val taskName: String, task: => A)

object HexTask {
  def apply[A](taskName: String)(task: => A) = new HexTask(taskName, task)
}