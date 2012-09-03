package edu.osu.compgeom.ayla

abstract class ProgressTracker(val taskCount: Int) {
  def setProgress(progress: Int)
  def setStatus(s: String)

  def update(progress: Int, status: String) = {
    setProgress(progress)
    setStatus(status)
  }
}