package edu.osu.compgeom.omegavis

import java.awt.Color
import edu.osu.compgeom.color.DivergingColormap
import javax.vecmath.Color4f
import edu.osu.compgeom.topology.ScalarFunction
import edu.osu.compgeom.ct.ContourTreeEdge

abstract class GammaColormap(val sf: ScalarFunction) {
	def rangeMax: Float = sf.maxFuncVal
	def rangeMin: Float = sf.minFuncVal
	def getColor(e: ContourTreeEdge, alpha: Double): Color4f
	def getColor(f: Float): Color4f
}

class JetGammaColormap(sf: ScalarFunction, transparency: Float = 1f) extends GammaColormap(sf) {
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

class GrayGammaColormap(sf: ScalarFunction, transparency: Float = 1f) extends GammaColormap(sf) {
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

class DivergingGammaColormap(sf: ScalarFunction, cmap: DivergingColormap, transparency: Float = 1f) extends GammaColormap(sf) {
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

class ProteinSimulationTypeColormap(sf: ScalarFunction) extends GammaColormap(sf) {
	def getColor(e: ContourTreeEdge, alpha: Double): Color4f = {
		val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
		if (nodeIDs.contains(GammaColormaps.idxDockingConformation)) {
			return new Color4f(Color.yellow)
		} else {
			val ntpmdCount = nodeIDs.count(_ < GammaColormaps.ofsREMD)
			val remdCount = nodeIDs.count(id => id >= GammaColormaps.ofsREMD && id < GammaColormaps.ofsREMD_new)
			val remdNewCount = nodeIDs.count(_ >= GammaColormaps.ofsREMD_new)
			
			val pctRegularMD = ntpmdCount / GammaColormaps.totalNTPMD.toDouble
			val pctREMD = remdCount / GammaColormaps.totalREMD.toDouble
			val pctREMDNew = remdNewCount / GammaColormaps.totalREMD_new.toDouble
			
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

class ProteinGammaColormap(minCount: Int, maxCount: Int, isInROI: Int => Boolean, sf: ScalarFunction) extends GammaColormap(sf) {
	override def rangeMax = maxCount.toFloat
	override def rangeMin = minCount.toFloat
	
	val cmInterp = new edu.osu.compgeom.color.DivergingColormap(Color.blue, Color.red)
	def getColor(e: ContourTreeEdge, alpha: Double): Color4f = {
		var count = 0
		e.noncriticalNodes.foreach(n => {
			if (n.vertex == GammaColormaps.idxDockingConformation)
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

object GammaColormaps {
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
		
		val cmInterp = new edu.osu.compgeom.color.DivergingColormap(Color.blue, Color.red)
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