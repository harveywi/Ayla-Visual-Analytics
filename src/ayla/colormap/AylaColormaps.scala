/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.colormap

import java.awt.Color
import javax.vecmath.Color4f
import ayla.geometry.ScalarFunction
import ayla.geometry.ct.ContourTreeEdge

abstract class AylaColormap(val sf: ScalarFunction) {
	def rangeMax: Float = sf.maxFuncVal
	def rangeMin: Float = sf.minFuncVal
	def getColor(e: ContourTreeEdge, alpha: Double): Color4f
	def getColor(f: Float): Color4f
}

class JetAylaColormap(sf: ScalarFunction, transparency: Float = 1f) extends AylaColormap(sf) {
	def getColor(e: ContourTreeEdge, alpha: Double): Color4f = {
		val fInterp = sf.getFuncVal(e.n1.vertex) * alpha + sf.getFuncVal(e.n2.vertex) * (1 - alpha)
		return getColor(fInterp.toFloat)
	}
	
	def getColor(f: Float): Color4f = {
		val c = colormap.Colormaps.getColor(f, sf.minFuncVal, sf.rangeFuncVal, colormap.Colormaps.CmapType.JET)
		val ret = new Color4f(c)
		ret.w = transparency
		return ret
	}
}

class GrayAylaColormap(sf: ScalarFunction, transparency: Float = 1f) extends AylaColormap(sf) {
	def getColor(e: ContourTreeEdge, alpha: Double): Color4f = {
		val fInterp = sf.getFuncVal(e.n1.vertex) * alpha + sf.getFuncVal(e.n2.vertex) * (1 - alpha)
		return getColor(fInterp.toFloat)
	}
	
	def getColor(f: Float): Color4f = {
		val c = colormap.Colormaps.getColor(f, sf.minFuncVal, sf.rangeFuncVal, colormap.Colormaps.CmapType.GRAY)
		val ret = new Color4f(c)
		ret.w = transparency
		return ret
	}
}

class DivergingAylaColormap(sf: ScalarFunction, cmap: DivergingColormap, transparency: Float = 1f) extends AylaColormap(sf) {
	def getColor(e: ContourTreeEdge, alpha: Double): Color4f = {
		val fInterp = sf.getFuncVal(e.n1.vertex) * alpha + sf.getFuncVal(e.n2.vertex) * (1 - alpha)
		return getColor(fInterp.toFloat)
	}
	
	def getColor(f: Float) = {
	  val c = new Color4f(cmap.interpolateDiverging((f - sf.minFuncVal) / sf.rangeFuncVal.toDouble))
	  c.w = transparency
	  c
	}
}

class ProteinSimulationTypeColormap(sf: ScalarFunction) extends AylaColormap(sf) {
	def getColor(e: ContourTreeEdge, alpha: Double): Color4f = {
		val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
		if (nodeIDs.contains(AylaColormaps.idxDockingConformation)) {
			return new Color4f(Color.yellow)
		} else {
			val ntpmdCount = nodeIDs.count(_ < AylaColormaps.ofsREMD)
			val remdCount = nodeIDs.count(id => id >= AylaColormaps.ofsREMD && id < AylaColormaps.ofsREMD_new)
			val remdNewCount = nodeIDs.count(_ >= AylaColormaps.ofsREMD_new)
			
			val pctRegularMD = ntpmdCount / AylaColormaps.totalNTPMD.toDouble
			val pctREMD = remdCount / AylaColormaps.totalREMD.toDouble
			val pctREMDNew = remdNewCount / AylaColormaps.totalREMD_new.toDouble
			
			val s = pctRegularMD + pctREMD + pctREMDNew
			val r = pctRegularMD / s
			val g = pctREMD / s
			val b = pctREMDNew / s
			
			
			// Set saturation based on function value
			val hsb = Color.RGBtoHSB((r*255).toInt, (g*255).toInt, (b*255).toInt, null)
			val fInterp = sf.getFuncVal(e.n1.vertex) * alpha + sf.getFuncVal(e.n2.vertex) * (1 - alpha)
			/*
			val saturation = .5 + .5 * (fInterp - sf.minFuncVal) / sf.rangeFuncVal.toDouble
			*/
			val saturation = 1f
			
			val rgbNew = new Color(Color.HSBtoRGB(hsb(0), saturation.toFloat, hsb(2)))
			val ret = new Color4f(rgbNew)
//			ret.w = if (pctRegularMD > 0) 1 else .2f
			return ret
		}
	}
	
	def getColor(f: Float): Color4f = {
		// TODO: not yet supported
		return new Color4f(.4f, .4f, .4f, .2f)
	}
}

class ProteinAylaColormap(minCount: Int, maxCount: Int, isInROI: Int => Boolean, sf: ScalarFunction) extends AylaColormap(sf) {
	override def rangeMax = maxCount.toFloat
	override def rangeMin = minCount.toFloat
	
	val cmInterp = new DivergingColormap(Color.blue, Color.red)
	def getColor(e: ContourTreeEdge, alpha: Double): Color4f = {
		var count = 0
		e.noncriticalNodes.foreach(n => {
			if (n.vertex == AylaColormaps.idxDockingConformation)
				return new Color4f(Color.yellow)
			else if (isInROI(n.vertex))
				count += 1
		})
		
		if (isInROI(e.n1.vertex))
			count += 1
		
		if (isInROI(e.n2.vertex))
			count += 1
		
		val c = if (count != 0) cmInterp.interpolateDiverging((count - minCount) / (maxCount - minCount).toDouble) else (new Color(40, 40, 40))
		val ret = new Color4f(c)
		ret.w = if (count > 0) 1 else .2f
		return ret
	}
	
	def getColor(f: Float): Color4f = new Color4f(cmInterp.interpolateDiverging((f - minCount) / (maxCount - minCount).toDouble))
}

object AylaColormaps {
	val ofsNTPMD = 0
	val totalNTPMD = 437
	val ofsREMD = 437
	val totalREMD = 20000
	val ofsREMD_new = ofsREMD + totalREMD
	val totalREMD_new = 232559
	
	val rangeNTPMD = Range(ofsNTPMD, totalNTPMD)
	val rangeREMD = Range(ofsREMD, ofsREMD + totalREMD)
	val rangeREMD_new = Range(ofsREMD_new, ofsREMD_new + totalREMD_new)

	val idxDockingConformation = ofsREMD + 4615
	
	def jet(sf: ScalarFunction)(e: ContourTreeEdge, alpha: Double): Color4f = {
//		val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
		val fInterp = sf.getFuncVal(e.n1.vertex) * alpha + sf.getFuncVal(e.n2.vertex) * (1 - alpha)
		val c = colormap.Colormaps.getColor(fInterp.toFloat, sf.minFuncVal, sf.rangeFuncVal, colormap.Colormaps.CmapType.JET)
		val ret = new Color4f(c)
		ret.w = 1f
		return ret
	}

	def divergingColormap(sf: ScalarFunction, cmap: DivergingColormap)(e: ContourTreeEdge, alpha: Double): Color4f = {
//		val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
		val fInterp = sf.getFuncVal(e.n1.vertex) * alpha + sf.getFuncVal(e.n2.vertex) * (1 - alpha)
		return new Color4f(cmap.interpolateDiverging((fInterp - sf.minFuncVal) / sf.rangeFuncVal.toDouble))
	}
	
	def typesColorMap(sf: ScalarFunction)(e: ContourTreeEdge, alpha: Double): Color4f = {
		val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
		if (nodeIDs.contains(idxDockingConformation)) {
			return new Color4f(Color.yellow)
		} else {
			val ntpmdCount = nodeIDs.count(_ < ofsREMD)
			val remdCount = nodeIDs.count(id => id >= ofsREMD && id < ofsREMD_new)
			val remdNewCount = nodeIDs.count(_ >= ofsREMD_new)
			
			val pctRegularMD = ntpmdCount / totalNTPMD.toDouble
			val pctREMD = remdCount / totalREMD.toDouble
			val pctREMDNew = remdNewCount / totalREMD_new.toDouble
			
			val s = pctRegularMD + pctREMD + pctREMDNew
			val r = pctRegularMD / s
			val g = pctREMD / s
			val b = pctREMDNew / s
			
			// Set saturation based on function value
			val hsb = Color.RGBtoHSB((r*255).toInt, (g*255).toInt, (b*255).toInt, null)
			val fInterp = sf.getFuncVal(e.n1.vertex) * alpha + sf.getFuncVal(e.n2.vertex) * (1 - alpha)
			val saturation = .5 + .5 * (fInterp - sf.minFuncVal) / sf.rangeFuncVal.toDouble
			
			val rgbNew = new Color(Color.HSBtoRGB(hsb(0), saturation.toFloat, hsb(2)))
			val ret = new Color4f(rgbNew)
			ret.w = if (pctRegularMD > 0) 1 else .2f
			return ret
		}
	}
	
	def ntpmdColorMap(minCount: Int, maxCount: Int, rangeOfInterest: Range)(sf: ScalarFunction)(e: ContourTreeEdge, alpha: Double): Color4f = {
		var count = 0
		e.noncriticalNodes.foreach(n => {
			if (n.vertex == idxDockingConformation)
				return new Color4f(Color.yellow)
			else if (rangeOfInterest.contains(n.vertex))
				count += 1
		})
		
		if (rangeOfInterest.contains(e.n1.vertex))
			count += 1
		
		if (rangeOfInterest.contains(e.n2.vertex))
			count += 1
		
		val cmInterp = new DivergingColormap(Color.blue, Color.red)
		val c = if (count != 0) cmInterp.interpolateDiverging((count - minCount) / (maxCount - minCount).toDouble) else (new Color(40, 40, 40))
		val ret = new Color4f(c)
		ret.w = if (count > 0) 1 else .2f
		return ret
		
		/*
		val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
		if (nodeIDs.contains(idxDockingConformation)) {
			return new Color4f(Color.yellow)
		} else {
			val count = nodeIDs.count(rangeOfInterest.contains)
			
			val cmInterp = new edu.osu.compgeom.color.DivergingColormap(Color.blue, Color.red)
			val c = if (count != 0) cmInterp.interpolateDiverging((count - minCount) / (maxCount - minCount).toDouble) else (new Color(40, 40, 40))
			val ret = new Color4f(c)
			ret.w = if (count > 0) 1 else .2f
			return ret
		}
		*/
	}
	
	def remdColorMap(sf: ScalarFunction)(e: ContourTreeEdge, alpha: Double): Color4f = {
		val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
		if (nodeIDs.contains(idxDockingConformation)) {
			return new Color4f(Color.yellow)
		} else {
			val ntpmdCount = nodeIDs.count(_ < ofsREMD)
			val remdCount = nodeIDs.count(id => id >= ofsREMD && id < ofsREMD_new)
			val remdNewCount = nodeIDs.count(_ >= ofsREMD_new)
			
			val pctRegularMD = ntpmdCount / totalNTPMD.toDouble
			val pctREMD = remdCount / totalREMD.toDouble
			val pctREMDNew = remdNewCount / totalREMD_new.toDouble
			
//			val s = pctRegularMD + pctREMD + pctREMDNew
//			val r = pctRegularMD / s
//			val g = .5//pctREMD / s
//			val b = .5//pctREMDNew / s
			
			// Set saturation based on function value
//			val hsb = Color.RGBtoHSB((r*255).toInt, (g*255).toInt, (b*255).toInt, null)
//			val fInterp = sf.getFuncVal(e.n1.vertex) * alpha + sf.getFuncVal(e.n2.vertex) * (1 - alpha)
//			val saturation = .5 + .5 * (fInterp - sf.minFuncVal) / sf.rangeFuncVal.toDouble
			
			val hue = Color.RGBtoHSB(255, 0, 0, null)(0)
			val rgb = new Color(Color.HSBtoRGB(hue, pctREMD.toFloat, 1f))
			val ret = new Color4f(rgb)
			ret.z = if (pctREMD > 0) 1 else .2f
			return ret
		}
	}
}
