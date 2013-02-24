/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui

import java.awt.Color
import ayla.client.ui.state.UIState

object ColorSchemes {
//  class ColorScheme(val lineColor: Color, val ssColorMap: Map[String, Color], val bgColor: Color, val btnForeground: Color, val btnBackground: Color)
  val dark = new ColorScheme("Dark",
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

  val light = new ColorScheme("Light",
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

//  var scheme = dark
//  private[this] var _scheme = dark
  
  def scheme = UIState.colorScheme
  
}
