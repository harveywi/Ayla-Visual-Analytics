package edu.osu.compgeom.ayla.event

import scala.swing.event._
import edu.osu.compgeom.ayla.collab.{ConformationAnnotation, Storyboard}

case class AnnotationsRefreshed(annotations: Array[ConformationAnnotation]) extends Event
case class StoryboardsRefreshed(annotations: Array[Storyboard]) extends Event

case object RefreshAnnotationVisibilities extends Event