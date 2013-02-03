/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui

import java.awt.Graphics
import org.jmol.adapter.smarter.SmarterJmolAdapter
import java.awt.Rectangle
import java.awt.Dimension
import org.jmol.api.JmolViewer
import javax.swing.JPanel
import java.awt.geom.Ellipse2D
import java.io._
import org.apache.commons.compress.archivers.zip._

class JmolPanel extends JPanel {
  val viewer = JmolViewer.allocateViewer(this, new SmarterJmolAdapter(),
    null, null, null, null, null).asInstanceOf[org.jmol.viewer.Viewer]
    
  val currentSize = new Dimension()
  val rectClip = new Rectangle()

  viewer.script("background [%d, %d, %d];".format(ColorSchemes.scheme.bgColor.getRed, ColorSchemes.scheme.bgColor.getGreen, ColorSchemes.scheme.bgColor.getBlue))

  def showConformation(pdbLines: Array[String]): Unit = {
    
//    val zipFile = new ZipFile("/home/harveywi/research/Protein/survivin/Regular-MD-Explicit/testZip.zip")
//    val entry = zipFile.getEntry("asdf/1E31_5_117_NTP_MD.pdb.1")
//    val zipInputStream = zipFile.getInputStream(entry)
//    val br = new BufferedReader(new InputStreamReader(zipInputStream))
//    val lines = Stream.continually(br.readLine).takeWhile(_ != null).toArray
    
//    val tempOutFile = File.createTempFile("protein", "pdb")
//    println(tempOutFile.getAbsolutePath())
//    val tempOutStream = new BufferedOutputStream(new FileOutputStream(tempOutFile))
//    val buf = new Array[Byte](1024)
//    var len = zipInputStream.read(buf)
//    while (len > 0) {
//      tempOutStream.write(buf, 0, len)
//      len = zipInputStream.read(buf)
//    }
//    tempOutStream.flush()
//    tempOutStream.close()
//    zipInputStream.close()
//    
//    println(tempOutFile.getAbsolutePath())
//    viewer.openFile(tempOutFile.getAbsolutePath())
//    tempOutFile.delete()
//    zipFile.close()
    
    viewer.zap(true, true, false)
    viewer.loadInline(pdbLines.mkString("\n"), '\n', false, null)
//    viewer.loadInline
    
//    val zipFile = new ZipFile("/home/harveywi/research/Protein/survivin/Regular-MD-Explicit")
//    val entry = zipFile.getEntry("1E31_5_117_NTP_MD.pdb.1")
//    val ois = new ObjectInputStream(zipFile.getInputStream(entry))
//    viewer.loadModelFromFile(null, null, null, new BufferedReader(new InputStreamReader(zipFile.getInputStream(entry))), false, null, null, 0)
    
    val bgColor = ColorSchemes.scheme.bgColor
    viewer.script("background [%d, %d, %d]; cartoons only; set antialiasDisplay on; color structure;".format(bgColor.getRed, bgColor.getGreen, bgColor.getBlue))
    for ((structElem, color) <- ColorSchemes.scheme.ssColorMap) {
      val script = "select %s; color [%d, %d, %d]".format(structElem, color.getRed, color.getGreen, color.getBlue)
      viewer.script(script)
    }
  }
  
  def compareConformations(pdbLines1: Array[String], pdbLines2: Array[String]): Unit = {
    viewer.loadInline((pdbLines1).mkString("\n"), '\n', false, null)
    viewer.loadInline((pdbLines2).mkString("\n"), '\n', true, null)
    val bgColor = ColorSchemes.scheme.bgColor
    viewer.script("background [%d, %d, %d]; cartoons only; set antialiasDisplay on; color structure;".format(bgColor.getRed, bgColor.getGreen, bgColor.getBlue))
    for ((structElem, color) <- ColorSchemes.scheme.ssColorMap) {
      val script = "select %s; color [%d, %d, %d]".format(structElem, color.getRed, color.getGreen, color.getBlue)
      viewer.script(script)
    }
    viewer.script("model all; compare {2.1} {1.1} rotate translate;")
  }

  override def paint(g: Graphics): Unit = {
    getSize(currentSize)
    g.getClipBounds(rectClip)
    viewer.renderScreenImage(g, currentSize, rectClip)
  }
}
