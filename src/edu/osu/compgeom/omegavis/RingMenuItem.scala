package edu.osu.compgeom.omegavis
import scala.swing.Publisher
import scala.swing.event.Event

class RingMenuItem(val text: String) extends Publisher {
  
  def handleClick(progressListener: RingMenuProgressListener): Unit = {}
  
//  override def toString = text + "(" + this + ")"
  
}