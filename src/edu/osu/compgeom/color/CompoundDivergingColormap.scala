package edu.osu.compgeom.color

import java.awt.Color

class CompoundDivergingColormap(colors: Array[Color]) {
	require(colors.size > 1)
	
	def cmaps = colors.zip(colors.drop(1)).map{case (c1, c2) => {
		new DivergingColormap(c1, c2)
	}}
	
	def interpolateDiverging(sIn: Double): Color = {
		require(sIn >= 0 && sIn <= 1)
		
		// Determine which diverging colormap to use
		val cmapToUse = cmaps(math.min(math.floor(sIn * cmaps.size).toInt, cmaps.size-1))
		return cmapToUse.interpolateDiverging(sIn)
	}
	
	
}