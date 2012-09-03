package edu.osu.compgeom.omegavis

import java.awt.Color

object ColorSchemes {
  class ColorScheme(val lineColor: Color, val ssColorMap: Map[String, Color], val bgColor: Color, val btnForeground: Color, val btnBackground: Color)
  private val dark = new ColorScheme(
    lineColor = new Color(100, 100, 140),
    ssColorMap = Map(
      "structure=-1" -> new Color(40, 40, 40), // non-protein parts (ligands)
      "structure=0" -> Color.decode("#787878"), // unstructure protein (random coil)
      "helixalpha" -> Color.decode("#5b03a0"),
      "helix310" -> Color.decode("#ffa000"),
      "helixpi" -> Color.decode("#5674f4"),
      "sheet" -> Color.decode("#a40f00"),
      "turn" -> Color.decode("#282828")),
    bgColor = Color.black,
    btnForeground = new Color(177, 186, 217),
    btnBackground = new Color(10, 10, 10))

  private val light = new ColorScheme(
    lineColor = new Color(100, 100, 140),
    ssColorMap = Map(
      "structure=-1" -> new Color(40, 40, 40), // non-protein parts (ligands)
      "structure=0" -> Color.decode("#787878"), // unstructure protein (random coil)
      "helixalpha" -> Color.decode("#5b03a0"),
      "helix310" -> Color.decode("#ffa000"),
      "helixpi" -> Color.decode("#5674f4"),
      "sheet" -> Color.decode("#a40f00"),
      "turn" -> Color.decode("#282828")),
    bgColor = Color.white,
    btnForeground = new Color(10, 10, 10),
    btnBackground = new Color(177, 186, 217))

  val scheme = dark
}