/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui

import scala.swing._
import java.awt.{ Dimension, Color, Point }
import ayla.geometry._
import ayla.geometry.ct._
import java.awt.geom._
import java.awt.RenderingHints
import java.text.DecimalFormat
import scala.swing.event._
import java.awt.BasicStroke

class SimplificationDialog(val ct: ContourTree) extends Dialog {

  /**
   * Dialog result goes here
   */
  var simpThreshold: Option[Float] = None

  title = "Topological Simplification"
  modal = true

  val histPanel = new Component {
    listenTo(mouse.moves)
    listenTo(mouse.clicks)
    preferredSize = new Dimension(800, 600)
    val decFormat = new DecimalFormat("00.00E0")

    val pairs = ct.scalarFunction.getStandardPersistencePairs()
    val minPersistence = pairs.minBy(_.persistence).persistence
    val maxPersistence = pairs.maxBy(_.persistence).persistence
    val rangePersistence = maxPersistence - minPersistence

    // Mouse tracking stuff
    val mousePos = new Point()

    val histPad = 50

    val kde = {
      val radius = 10
      val nBins = 1000
      val bins = new Array[Double](nBins)
      pairs.foreach { pair =>
        val binID = ((pair.persistence - minPersistence) / rangePersistence) * (bins.size - 1)
        bins(binID.toInt) += 1
      }
      val kde = new Array[Double](bins.size)
      bins.indices.foreach(i => {
        (math.max(0, i - radius) to math.min(i + radius, bins.size - 1)).foreach { j =>
          val u = (j - i) / radius.toDouble
          kde(i) += .75 * (1 - u * u) * bins(j)
        }
      })

      // Normalize bins and kde to have max height = 1
      val heightBins = bins.max
      val heightEpanechnikov = kde.max
      bins.indices.foreach(i => bins(i) = bins(i) / heightBins)
      kde.indices.foreach(i => kde(i) = kde(i) / heightEpanechnikov)
      kde
    }

    reactions += {
      case e: MouseMoved => {
        mousePos.move(e.point.x, e.point.y)
        repaint
      }
      case e: MousePressed => {
        val histRect = new Rectangle(histPad, histPad, bounds.width - 2 * histPad, bounds.height - 2 * histPad)
        if (histRect.contains(e.point)) {
          simpThreshold = Some((minPersistence + (e.point.x - histRect.x) / histRect.width.toDouble * rangePersistence).toFloat)
          okButton.enabled = true
          repaint
        }
      }
    }

    override def paintComponent(g2d: Graphics2D) = {
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2d.setColor(Color.black)
      g2d.fillRect(0, 0, bounds.width, bounds.height)

      // Rectangle where the (non-axes) part of the histogram will go
      val histRect = new Rectangle(histPad, histPad, bounds.width - 2 * histPad, bounds.height - 2 * histPad)
      drawHistogram(g2d, histRect)

      if (simpThreshold.isDefined) {
        val thresh = simpThreshold.get
        val x = (histRect.x + (thresh - minPersistence) / rangePersistence * histRect.width).toInt
        val oldStroke = g2d.getStroke
        g2d.setStroke(new BasicStroke(2.0f))
        val color = Color.orange.brighter()
        g2d.setColor(color)
        g2d.drawLine(x, histRect.y, x, histRect.y + histRect.height - 2)
        g2d.setStroke(oldStroke)
      }

      if (histRect.contains(mousePos)) {
        // Draw mouse vertical bar
        val dashed = new BasicStroke(2.0f, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10.0f, Array(10f), 0f)
        val oldStroke = g2d.getStroke
        g2d.setStroke(dashed)
        g2d.setColor(Color.orange)
        g2d.drawLine(mousePos.x, histRect.y, mousePos.x, histRect.y + histRect.height)
        g2d.setStroke(oldStroke)
      }
    }

    def drawHistogram(g2d: Graphics2D, rect: Rectangle) {
      val gridSpacing = 50
      val gridColor = new Color(50, 50, 70)
      g2d.setColor(gridColor)

      (rect.x until (rect.x + rect.width) by gridSpacing).foreach(x => g2d.drawLine(x, rect.y, x, rect.y + rect.height))
      ((rect.y + rect.height) until rect.y by -gridSpacing).foreach(y => g2d.drawLine(rect.x, y, rect.x + rect.width, y))

      val gp = new GeneralPath
      gp.moveTo(rect.x, rect.y + rect.height - 1)
      val histTop = (0 until rect.width).foreach { x =>

        val binNumber = ((x / rect.width.toDouble) * kde.size).toInt
        val ky = ((1 - kde(binNumber)) * rect.height).toInt
        gp.lineTo(rect.x + x, rect.y + ky)
      }
      gp.lineTo(rect.x + rect.width, rect.y + rect.height - 1)
      gp.closePath()
      g2d.setColor(new Color(40, 40, 40, 128))
      g2d.fill(gp)
      g2d.setColor(new Color(158, 188, 218))
      g2d.draw(gp)

      g2d.setColor(gridColor)
      g2d.drawRect(rect.x - 1, rect.y - 1, rect.width + 2, rect.height + 2)

      g2d.setColor(new Color(90, 90, 120))
      (rect.x until (rect.x + rect.width) by gridSpacing).foreach { x =>
        val persistence = minPersistence + (x - rect.x) / rect.width.toDouble * rangePersistence
        val s = decFormat.format(persistence)

        val oldTrans = g2d.getTransform()

        g2d.translate(x - 8, rect.y + rect.height + 9)
        g2d.rotate(math.Pi / 4.0)
        g2d.drawString(s, 0, 0)

        g2d.setTransform(oldTrans)
      }

      val oldTrans = g2d.getTransform()
      val ps = "Persistence Frequency"
      val psBounds = getStringDims(g2d, ps)
      g2d.translate(rect.x / 2 + psBounds.height / 2, (rect.y + rect.height) / 2 + psBounds.width / 2)
      g2d.rotate(-math.Pi / 2.0)
      g2d.drawString(ps, 0, 0)
      g2d.setTransform(oldTrans)
    }

    def getStringDims(g2d: Graphics2D, s: String): java.awt.Dimension = {
      val fontMetrics = g2d.getFontMetrics
      val layout = new java.awt.font.TextLayout(s, g2d.getFont, g2d.getFontRenderContext)
      val bounds = layout.getBounds
      new Dimension(bounds.getWidth.toInt, bounds.getHeight.toInt)
    }

  }

  val okButton = new Button("OK") {
    enabled = false
    background = Color.black
    foreground = new Color(158, 188, 218)
    reactions += {
      case e: ButtonClicked => {
        close()
      }
    }
  }

  contents = new BorderPanel {
    add(histPanel, BorderPanel.Position.Center)
    add(new FlowPanel() {
      background = Color.black
      contents += okButton
      contents += new Button("Cancel") {
        background = Color.black
        foreground = new Color(158, 188, 218)
        reactions += {
          case e: ButtonClicked => {
            simpThreshold = None
            close()
          }
        }
      }
    }, BorderPanel.Position.South)
  }

  open()
}
