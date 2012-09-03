package edu.osu.compgeom.ayla.collab

import java.util.Date

@SerialVersionUID(1L)
class Storyboard(val name: String, val annotations: Array[ConformationAnnotation]) extends Serializable {
  val timestamp = new Date
  override def toString = "%s [%s]".format(name, timestamp.toString)
}