package edu.osu.compgeom.color

import java.awt.Color
import java.awt._

class DivergingColormap(c1: Color, c2: Color) {
  val rgb1 = Array(c1.getRed / 255.0, c1.getGreen / 255.0, c1.getBlue / 255.0)
  val rgb2 = Array(c2.getRed / 255.0, c2.getGreen / 255.0, c2.getBlue / 255.0)

  def interpolateDiverging(sIn: Double): Color = {
    val lab1 = rgbToLab(rgb1(0), rgb1(1), rgb1(2))
    val lab2 = rgbToLab(rgb2(0), rgb2(1), rgb2(2))

    val msh1 = labToMsh(lab1)
    val msh2 = labToMsh(lab2)

    var s = sIn

    if ((msh1(1) > .05) && (msh2(1) > .05) && (angleDiff(msh1(2), msh2(2)) > .33 * math.Pi)) {
      val Mmid = scala.List(msh1(0), msh2(0), 88).max
      if (s < .5) {
        msh2(0) = Mmid
        msh2(1) = 0
        msh2(2) = 0
        s = 2 * s
      } else {
        msh1(0) = Mmid
        msh1(1) = 0
        msh1(2) = 0
        s = 2 * s - 1
      }
    }

    if ((msh1(1) < .05) && (msh2(1) > .05)) {
      msh1(2) = adjustHue(msh2, msh1(0))
    } else if ((msh2(1) < .05) && (msh1(1) > .05)) {
      msh2(2) = adjustHue(msh1, msh2(0))
    }

    val mshTmp = msh1.zip(msh2).map { case (x1, x2) => (1 - s) * x1 + s * x2 }
    val labTmp = mshToLab(mshTmp)
    val finalRGB = labToRGB(labTmp)
    val ret = new Color(finalRGB(0).toFloat, finalRGB(1).toFloat, finalRGB(2).toFloat)
    return ret
  }

  private def labToRGB(lab: Array[Double]): Array[Double] = {
    val xyz = labToXYZ(lab(0), lab(1), lab(2))
    return XYZToRGB(xyz(0), xyz(1), xyz(2))
  }

  private def XYZToRGB(x: Double, y: Double, z: Double): Array[Double] = {
    val r1 = x * 3.2406 + y * -1.5372 + z * -0.4986
    val g1 = x * -0.9689 + y * 1.8758 + z * 0.0415
    val b1 = x * 0.0557 + y * -0.2040 + z * 1.0570

    def gamma(x: Double): Double = {
      if (x > 0.0031308)
        return 1.055 * math.pow(x, 1 / 2.4) - 0.055
      else
        return 12.92 * x
    }

    var (r, g, b) = (gamma(r1), gamma(g1), gamma(b1))

    val maxVal = scala.List(r, g, b).max
    if (maxVal > 1.0) {
      r /= maxVal
      g /= maxVal
      b /= maxVal
    }
    if (r < 0)
      r = 0
    if (g < 0)
      g = 0
    if (b < 0)
      b = 0

    return Array(r, g, b)

  }

  private def labToXYZ(L: Double, a: Double, b: Double): Array[Double] = {
    val vY = (L + 16) / 116
    val vX = a / 500 + vY
    val vZ = vY - b / 200

    def convert(vt: Double): Double = {
      if (math.pow(vt, 3) > 0.008856)
        return math.pow(vt, 3)
      else
        return (vt - 16.0 / 116.0) / 7.787
    }

    val (var_X, var_Y, var_Z) = (convert(vX), convert(vY), convert(vZ))
    val (ref_X, ref_Y, ref_Z) = (0.9505, 1.0, 1.089)

    return Array(ref_X * var_X, ref_Y * var_Y, ref_Z * var_Z)

  }

  private def mshToLab(msh: Array[Double]): Array[Double] = {
    val M = msh(0)
    val s = msh(1)
    val h = msh(2)
    return Array(M * math.cos(s), M * math.sin(s) * math.cos(h), M * math.sin(s) * math.sin(h))
  }

  private def adjustHue(msh: Array[Double], unsatM: Double): Double = {
    if (msh(0) >= unsatM - 0.1)
      return msh(2)
    else {
      val hueSpin = (msh(1) * math.sqrt(unsatM * unsatM - msh(0) * msh(0)) / (msh(0) * math.sin(msh(1))))
      if (msh(2) > -0.3 * math.Pi)
        return msh(2) + hueSpin
      else
        return msh(2) - hueSpin
    }
  }

  private def angleDiff(a1: Double, a2: Double): Double = {
    var adiff = a1 - a2
    if (adiff < 0)
      adiff = -adiff

    while (adiff >= 2 * math.Pi)
      adiff -= 2 * math.Pi

    if (adiff > math.Pi)
      adiff = 2 * math.Pi - adiff
    return adiff
  }

  private def labToMsh(lab: Array[Double]): Array[Double] = {
    val L = lab(0)
    val a = lab(1)
    val b = lab(2)
    val m = math.sqrt(L * L + a * a + b * b)
    val s = if (m > 0.001) math.acos(L / m) else 0.0
    val h = if (s > 0.001) math.atan2(b, a) else 0.0
    return Array(m, s, h)
  }

  private def rgbToLab(red: Double, green: Double, blue: Double): Array[Double] = {
    val (x, y, z) = RGBtoXYZ(red, green, blue)
    return XYZToLab(x, y, z)
  }

  private def RGBtoXYZ(rIn: Double, gIn: Double, bIn: Double): (Double, Double, Double) = {
    def correct(v: Double): Double = {
      if (v > 0.04045)
        return math.pow((v + 0.055) / 1.055, 2.4)
      else
        return v / 12.92
    }

    val (r, g, b) = (correct(rIn), correct(gIn), correct(bIn))
    val x = r * 0.4124 + g * 0.3576 + b * 0.1805
    val y = r * 0.2126 + g * 0.7152 + b * 0.0722
    val z = r * 0.0193 + g * 0.1192 + b * 0.9505
    (x, y, z)
  }

  private def XYZToLab(x: Double, y: Double, z: Double): Array[Double] = {
    val (ref_X, ref_Y, ref_Z) = (0.9505, 1.000, 1.089)

    def convert(t: Double, ref_t: Double): Double = {
      val var_t = t / ref_t
      if (var_t > 0.008856)
        return math.pow(var_t, 1.0 / 3.0)
      else
        return (7.787 * var_t) + (16.0 / 116.0)
    }

    val (var_X, var_Y, var_Z) = (convert(x, ref_X), convert(y, ref_Y), convert(z, ref_Z))
    val L = (116 * var_Y) - 16
    val a = 500 * (var_X - var_Y)
    val b = 200 * (var_Y - var_Z)
    return Array(L, a, b)
  }
}

object DivergingColormap {
  def main(args: Array[String]): Unit = {
    println("Testing the colormap stuff")
    import java.awt.image.BufferedImage
    import javax.media.jai.JAI
    val img = new BufferedImage(100, 50, BufferedImage.TYPE_3BYTE_BGR)
    val g2d = img.getGraphics()
    val cmap = new DivergingColormap(Color.green, Color.red)
    (0 until 100).foreach { i =>
      val x = i / 99.0
      val c = cmap.interpolateDiverging(x)
      g2d.setColor(c)
      g2d.drawLine(i, 0, i, 50)
    }
    JAI.create("filestore", img, "/home/harveywi/check.png", "PNG")
  }
}
