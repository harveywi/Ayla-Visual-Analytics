package edu.osu.compgeom.omegavis

import java.awt.Graphics
import scala.swing._
import scala.swing.event._

import java.awt.{Color}
import java.awt.image.BufferedImage

class GammaHUD extends java.awt.Component {
	override def isOpaque = false
	
	override def paint(g: Graphics): Unit = {
		super.paint(g)
		g.setColor(Color.green)
		g.drawString("Hello world!", 50, 50)
		
	}
}