package edu.osu.compgeom.color

import java.awt.Color

class LinearInterpolatingColormap(colors: Array[Color]) {
	val binWidth = 1.0d / (colors.size-1)
	
	def interpolate(sIn: Double): Color = {
		val idx1 = (sIn / binWidth).toInt
		val idx2 = math.min(idx1 + 1, colors.size-1)
		val c1 = colors(idx1)
		val c2 = colors(idx2)
		
		val alpha = (sIn - idx1*binWidth) / binWidth
		
		val dr = c2.getRed - c1.getRed
		val dg = c2.getGreen - c1.getGreen
		val db = c2.getBlue - c1.getBlue
		
		val r = c1.getRed + (dr*alpha).toInt
		val g = c1.getGreen + (dg*alpha).toInt
		val b = c1.getBlue + (db*alpha).toInt
		
		new Color(r, g, b)
	}

}