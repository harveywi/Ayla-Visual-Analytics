/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui

import java.awt.{ Point, Color, Graphics, RenderingHints }
import java.awt.geom._
import scala.swing._
import javax.swing.BorderFactory
import java.awt.GridBagConstraints
import scala.swing.event.MouseMoved
import java.awt.BasicStroke
import scala.swing.event.MouseClicked
import javax.swing.JLayeredPane
import javax.swing.OverlayLayout
import scala.swing.event.UIElementResized
import java.awt.image.BufferedImage
import java.awt.Toolkit
import java.awt.Cursor
import scala.swing.event.Event
import java.io._
import scala.swing.event.{ MouseExited, ButtonClicked }
import ayla.client.ui.event.TopologicalComponentManagerSelectionUpdate
import javax.swing.ImageIcon
import org.artemis.progx.graphics.GraphicsUtilities
import javax.swing.SwingConstants
import ayla.util.IO._
import ayla.dataset.Dataset
import ayla.collab.CreateConformationAnnotationDialog
import javax.swing.OverlayLayout
import java.awt.BorderLayout
import ayla.client.ui.event.SetS1
import _root_.reactive._

class ConformationalSpacePanel extends BorderPanel with Reactor {
  var dataset: Dataset = null

  var stateManager: StateManager = null

  val debug = false

  val jmolPanel = new JmolPanel()

  val ssBar = new SecondaryStructureBar(SecondaryStructureBar.Bottom)

  // A little function to make a nice image icon for the color legend
  def makeImageIcon(color: Color): ImageIcon = {
    val img = GraphicsUtilities.createTranslucentCompatibleImage(16, 24)
    val g = img.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(color)
    val shape = new RoundRectangle2D.Double(2, 2, 12, 19, 5, 5)
    g.fill(shape)

    new ImageIcon(img)
  }

  val labelBG = ColorSchemes.scheme.bgColor
  val labelFG = ColorSchemes.scheme.btnForeground
  val key = new GridPanel(2, 3)
  key.contents += new Label("Alpha Helix") { opaque = true; icon = makeImageIcon(ColorSchemes.scheme.ssColorMap("helixalpha")); background = labelBG; foreground = labelFG; horizontalAlignment = Alignment.Left }
  key.contents += new Label("Beta Sheet") { opaque = true; icon = makeImageIcon(ColorSchemes.scheme.ssColorMap("sheet")); background = labelBG; foreground = labelFG; horizontalAlignment = Alignment.Left }
  key.contents += new Label("310 Helix") { opaque = true; icon = makeImageIcon(ColorSchemes.scheme.ssColorMap("helix310")); background = labelBG; foreground = labelFG; horizontalAlignment = Alignment.Left }
  key.contents += new Label("Pi Helix") { opaque = true; icon = makeImageIcon(ColorSchemes.scheme.ssColorMap("helixpi")); background = labelBG; foreground = labelFG; horizontalAlignment = Alignment.Left }
  key.contents += new Label("Turn") { opaque = true; icon = makeImageIcon(ColorSchemes.scheme.ssColorMap("turn")); background = labelBG; foreground = labelFG; horizontalAlignment = Alignment.Left }
  key.contents += new Label("Unstructured") { opaque = true; icon = makeImageIcon(ColorSchemes.scheme.ssColorMap("structure=0")); background = labelBG; foreground = labelFG; horizontalAlignment = Alignment.Left }

  val ssPanel = new SecondaryStructurePanel

  val ssBorderPanel = new BorderPanel {
    add(key, BorderPanel.Position.North)
    add(ssPanel, BorderPanel.Position.Center)
  }

  val procrustesContainer = new BorderPanel() {
    private[this] val conformationIDsToShow = new scala.collection.mutable.ArrayBuffer[Int]

    def updateSelectedConformations(newConformations: Iterable[Int]) = {
      refreshProcrustesButton.visible = true
      conformationIDsToShow.clear()
      procrustesPanel.setSelectedConformations(Array.empty[InputStream])
      conformationIDsToShow ++= newConformations
      refreshProcrustesButton.enabled = true
    }

    val procrustesPanel = new ProcrustesBackbonePanel

    val refreshProcrustesButton = new Button {
      background = ColorSchemes.scheme.btnBackground
      foreground = ColorSchemes.scheme.btnForeground
      text = "Show Procrustes Overlay"
      enabled = false

      reactions += {
        case e: ButtonClicked => {
          procrustesPanel.setSelectedConformations(conformationIDsToShow.map(conformationID => stateManager.dataset.getPDBInputStream(conformationID)).toArray)
        }
      }
    }

    add(procrustesPanel, BorderPanel.Position.Center)
    add(refreshProcrustesButton, BorderPanel.Position.South)
  }

  val pointsPanel = new PointsPanel

  val annotateButton = new Button("Annotate") with Observing {
    enabled = false
    listenTo(pointsPanel)
    foreground = ColorSchemes.scheme.btnForeground
    background = ColorSchemes.scheme.btnBackground
    var confID = -1
    var pdbLines: Array[String] = null

    reactions += {
      case e: ButtonClicked => {
        //stateManager.dataset
        CreateConformationAnnotationDialog.getAnnotation(confID, pdbLines, stateManager.terrainPanel.getCameraTransform) match {
          case Some(annotation) => {
            dataset.addAnnotation(annotation)
          }
          case None => {}
        }
      }
    }

    pointsPanel.EventStreams.conformationPointClicked.foreach { e =>
      enabled = true
      confID = e.conformationID
      pdbLines = e.pdbLines
    }
  }

  val gridPanel = new GridPanel(2, 2)
  //  gridPanel.contents += Component.wrap(jmolPanel)
  gridPanel.contents += new BorderPanel {
    add(Component.wrap(jmolPanel), BorderPanel.Position.Center)
    add(annotateButton, BorderPanel.Position.South)
  }
  gridPanel.contents += pointsPanel
  gridPanel.contents += procrustesContainer
  gridPanel.contents += ssBorderPanel

  add(ssBar, BorderPanel.Position.West)
  add(gridPanel, BorderPanel.Position.Center)

  ssBar.listenTo(pointsPanel)

  class SecondaryStructurePanel extends Component with Publisher {
    var bgImage = {
      val img = GraphicsUtilities.createCompatibleImage(1, 1)
      val g = img.createGraphics
      g.setColor(ColorSchemes.scheme.bgColor)
      g.fillRect(0, 0, 1, 1)
      img
    }

    override def paintComponent(g2d: Graphics2D): Unit = {
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
      g2d.setColor(ColorSchemes.scheme.bgColor)
      g2d.fillRect(0, 0, size.width, size.height)
      val heightScale = math.min(1, size.height / bgImage.getHeight.toDouble)
      g2d.scale(size.width / bgImage.getWidth.toDouble, heightScale)
      g2d.drawImage(bgImage, 0, 0, null)
    }

    reactions += {
      case TopologicalComponentManagerSelectionUpdate(tcManager) => {
        // Here we will make a new bgImage containing the colored bars.

        val dsspToJmolStructure = Map(
          'X' -> "structure=0",
          'H' -> "helixalpha",
          'B' -> "sheet",
          'E' -> "sheet",
          'G' -> "helix310",
          'I' -> "helixpi",
          'T' -> "turn",
          'S' -> "structure=0" /* 'S' -> "bend" */ )

        val numResidues = {
          // Get last line of some PDB file in the dataset, and grab the residue ID of it.
          // That will equal the number of residues in the protein
          val somePdb = stateManager.dataset.getPDBInputStream(0)
          val lastPdbLine = withBufferedReader(somePdb) { br => Stream.continually(br.readLine).takeWhile(_ != null).filter(_.startsWith("ATOM")).last }
          val lastResidueID = lastPdbLine.substring(22, 27).trim.toInt
          lastResidueID
        }

        val barWidth = 2
        val barHeight = 8
        val allBarLabels = new scala.collection.mutable.ListBuffer[Array[Char]]

        val selectedConformationIDs = new scala.collection.mutable.HashSet[Int]
        for ((ctEdge, tcEntry) <- tcManager.componentMap if tcEntry.selectionStatus != 0) {
          selectedConformationIDs += ctEdge.n1.vertex
          selectedConformationIDs += ctEdge.n2.vertex
          ctEdge.noncriticalNodes.foreach(n => selectedConformationIDs += n.vertex)
        }

        val sortedConformationIDs = selectedConformationIDs.toIndexedSeq.sortWith((id1, id2) => {
          val f1 = stateManager.morseFunction.getFuncVal(id1)
          val f2 = stateManager.morseFunction.getFuncVal(id2)
          f1 > f2
        })
        
        dataset.dsspProvider.foreach {dsspProvider =>
          val w = numResidues * barWidth
          sortedConformationIDs.foreach(id => {
            val barLabels = dsspProvider.getDSSPLabels(numResidues, id)
//            val barLabels = Array.fill[Char](numResidues)('X')
//            (0 until numResidues).foreach { i =>
//              val ofs = id * numResidues + i
//              barLabels(i) = dsspArray(ofs)
//            }
            allBarLabels += barLabels
          })

          if (allBarLabels.size == 0) {
            bgImage = GraphicsUtilities.createCompatibleImage(1, 1)
          } else {
            bgImage = GraphicsUtilities.createCompatibleImage(w, barHeight * allBarLabels.size)
            val g2d = bgImage.createGraphics

            g2d.setColor(ColorSchemes.scheme.bgColor)
            g2d.fillRect(0, 0, bounds.width, bounds.height)
            for ((barLabels, i) <- allBarLabels.zipWithIndex) {
              val y = i * barHeight
              for (j <- 0 until barLabels.size) {
                val jmolSS = dsspToJmolStructure.getOrElse(barLabels(j), dsspToJmolStructure('X'))
                val color = ColorSchemes.scheme.ssColorMap(jmolSS)
                val x = j * barWidth
                g2d.setColor(color)
                g2d.fillRect(x, y, barWidth, barHeight - 1)
              }
            }
          }
          repaint()
        }

        /*
        if (dataset.dsspOutput.isDefined) {
          val dsspArray = dataset.dsspOutput.get
          val w = numResidues * barWidth
          sortedConformationIDs.foreach(id => {
            val barLabels = Array.fill[Char](numResidues)('X')
            (0 until numResidues).foreach { i =>
              val ofs = id * numResidues + i
              barLabels(i) = dsspArray(ofs)
            }
            allBarLabels += barLabels
          })

          if (allBarLabels.size == 0) {
            bgImage = GraphicsUtilities.createCompatibleImage(1, 1)
          } else {
            bgImage = GraphicsUtilities.createCompatibleImage(w, barHeight * allBarLabels.size)
            val g2d = bgImage.createGraphics

            g2d.setColor(ColorSchemes.scheme.bgColor)
            g2d.fillRect(0, 0, bounds.width, bounds.height)
            for ((barLabels, i) <- allBarLabels.zipWithIndex) {
              val y = i * barHeight
              for (j <- 0 until barLabels.size) {
                val jmolSS = dsspToJmolStructure.getOrElse(barLabels(j), dsspToJmolStructure('X'))
                val color = ColorSchemes.scheme.ssColorMap(jmolSS)
                val x = j * barWidth
                g2d.setColor(color)
                g2d.fillRect(x, y, barWidth, barHeight - 1)
              }
            }
          }
          repaint()
        }
        */
      }
    }
  }

  class SecondaryStructureBar(snap: SecondaryStructureBar.VerticalSnap) extends Component {
    val barLabels = Array.fill(228)('X')

    val dsspToJmolStructure = Map(
      'X' -> "structure=0",
      'H' -> "helixalpha",
      'B' -> "sheet",
      'E' -> "sheet",
      'G' -> "helix310",
      'I' -> "helixpi",
      'T' -> "turn",
      'S' -> "structure=0" /* 'S' -> "bend" */ )

    override def paint(g2d: Graphics2D): Unit = {
      g2d.setColor(ColorSchemes.scheme.bgColor)
      g2d.fillRect(0, 0, bounds.width, bounds.height)

      val numBarsHoriz = 228 / 4
      for (barID <- 0 until 228) {
        val gx = barID % numBarsHoriz
        val gy = barID / numBarsHoriz

        val x = (gx * (bounds.width / numBarsHoriz.toDouble)).toInt
        val y = if (snap == SecondaryStructureBar.Top) {
          (gy * (bounds.height / 8.0)).toInt
        } else {
          (gy * (bounds.height / 8.0)).toInt + bounds.height / 2
        }

        val jmolSS = dsspToJmolStructure(barLabels(barID))
        val color = ColorSchemes.scheme.ssColorMap(jmolSS)

        g2d.setColor(color)
        g2d.fillRect(x + 1, y + 1, (bounds.width / numBarsHoriz.toDouble - 2).toInt, bounds.height / 8 - 2)
        g2d.setColor(color.darker)
        g2d.drawRect(x + 1, y + 1, (bounds.width / numBarsHoriz.toDouble - 2).toInt, bounds.height / 8 - 2)
      }
    }
  }
  object SecondaryStructureBar {
    sealed abstract class VerticalSnap
    case object Top extends VerticalSnap
    case object Bottom extends VerticalSnap
  }

  class PointsPanel extends Component {
    object EventStreams {
      val conformationPointClicked = new EventSource[ConformationPointClicked]
    }

    peer.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR))

    listenTo(mouse.clicks)
    listenTo(mouse.moves)
    listenTo(this)
    class ConformationPt(val conformationID: Int, val selectionID: Int) extends Point2D.Double()

    var nearestIdx = -1
    var s1 = -1
    val selectedConformations = new scala.collection.mutable.ArrayBuffer[ConformationPt]

    var tcManager: TopologicalComponentManager = null

    def setS1(conformationID: Int): Unit = {
      nearestIdx = selectedConformations.indexWhere(c => c.conformationID == conformationID)
      val c = selectedConformations(nearestIdx)
      val pdbLines = stateManager.dataset.getPDBLines(c.conformationID)
      jmolPanel.showConformation(pdbLines)
      s1 = nearestIdx
      EventStreams.conformationPointClicked.fire(ConformationPointClicked(c.conformationID, pdbLines))
      repaint()
    }

    reactions += {
      case SetS1(conformationID) => setS1(conformationID)

      case TopologicalComponentManagerSelectionUpdate(tcManager) => {
        this.tcManager = tcManager //this code is getting worse and worse...
        s1 = -1
        nearestIdx = -1
        // Extract the selected conformations from the topological components
        val selectedConformationMap = new scala.collection.mutable.HashMap[Int, ConformationPt]

        for ((ctEdge, tcEntry) <- tcManager.componentMap if tcEntry.selectionStatus != 0) {
          selectedConformationMap(ctEdge.n1.vertex) = new ConformationPt(ctEdge.n1.vertex, tcEntry.selectionStatus)
          selectedConformationMap(ctEdge.n2.vertex) = new ConformationPt(ctEdge.n2.vertex, tcEntry.selectionStatus)
          ctEdge.noncriticalNodes.foreach(n => {
            selectedConformationMap(n.vertex) = new ConformationPt(n.vertex, tcEntry.selectionStatus)
          })
        }

        selectedConformations.clear()
        selectedConformations ++= selectedConformationMap.values

        // Update the procrustes panel
        procrustesContainer.updateSelectedConformations(selectedConformations.toIterable.map(_.conformationID))
        //procrustesPanel.setSelectedConformations(selectedConformations.map(confPt => stateManager.dataset.getPDBInputStream(confPt.conformationID)).toArray)

        // Need to calculate positions of the conformations now.
        calcPointPositions()

        repaint()

      }

      case e: UIElementResized => {
        calcPointPositions()
        repaint()
      }

      case e: MouseExited => { nearestIdx = -1; repaint() }

      case e: MouseClicked => {
        if (nearestIdx >= 0 && nearestIdx < selectedConformations.size) {
          val c = selectedConformations(nearestIdx)
          val pdbLines = stateManager.dataset.getPDBLines(c.conformationID)
          jmolPanel.showConformation(pdbLines)
          s1 = nearestIdx
          EventStreams.conformationPointClicked.fire(ConformationPointClicked(c.conformationID, pdbLines))
          repaint()
        }
      }
      case e: MouseMoved => {
        if (selectedConformations.size != 0) {
          // For now, only paint in the center
          val pad = 3
          var nearestDistSq = Double.PositiveInfinity
          var newNearestIdx = -1
          selectedConformations.zipWithIndex.foreach {
            case (c, idx) => {
              val distSq = c.distanceSq(e.point)
              if (distSq < nearestDistSq) {
                nearestDistSq = distSq
                newNearestIdx = idx
              }
            }
          }
          if (newNearestIdx != nearestIdx) {
            nearestIdx = newNearestIdx
            repaint()
          }
        }
      }
    }

    def calcPointPositions(): Unit = {
      if (selectedConformations.size == 0)
        return

      //      val minX = selectedConformations.map(c => stateManager.dataset.pcaPoints(c.conformationID)(0)).min
      //      val maxX = selectedConformations.map(c => stateManager.dataset.pcaPoints(c.conformationID)(0)).max
      val minX = selectedConformations.map(c => stateManager.ctSimp.scalarFunction.vertices(c.conformationID)(0)).min
      val maxX = selectedConformations.map(c => stateManager.ctSimp.scalarFunction.vertices(c.conformationID)(0)).max
      val minY = selectedConformations.map(c => stateManager.morseFunction.maxFuncVal - stateManager.morseFunction.getFuncVal(c.conformationID)).min
      val maxY = selectedConformations.map(c => stateManager.morseFunction.maxFuncVal - stateManager.morseFunction.getFuncVal(c.conformationID)).max

      val rangeX = if (maxX != minX) maxX - minX else 1
      val rangeY = if (maxY != minY) maxY - minY else 1

      val pad = 20

      selectedConformations.foreach(c => {
        val x = stateManager.ctSimp.scalarFunction.vertices(c.conformationID)(0)
        val y = stateManager.morseFunction.maxFuncVal - stateManager.morseFunction.getFuncVal(c.conformationID)

        c.x = pad + (bounds.width - 2 * pad) * (x - minX) / rangeX
        c.y = pad + (bounds.height - 2 * pad) * (y - minY) / rangeY
      })
    }

    override def paint(g2d: Graphics2D): Unit = {
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2d.setColor(ColorSchemes.scheme.bgColor)
      g2d.fillRect(0, 0, bounds.width, bounds.height)

      selectedConformations.foreach(c => {
        val color = try { tcManager.colormap.getColor(stateManager.colorFunction.getFuncVal(c.conformationID)) } catch {
          case _: Exception => new javax.vecmath.Color4f(1f, 1f, 1f, 1f)
        }

        g2d.setColor(new Color(color.x, color.y, color.z, color.w))

        if (c == null) {
          println("Null found")
        }

        val x = c.x.toInt
        val y = c.y.toInt

        val oval = new Ellipse2D.Double(x - 2, y - 2, 4, 4)
        g2d.fill(oval)
      })

      if (s1 >= 0 && s1 < selectedConformations.size) {
        val c = selectedConformations(s1)
        val x = c.x.toInt
        val y = c.y.toInt
        g2d.setColor(TopologicalComponentManager.selectionGroupColorsAWT(1))
        g2d.setStroke(new BasicStroke(3, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
        val oval = new Ellipse2D.Double(x - 5, y - 5, 9, 9)
        g2d.draw(oval)
      }

      g2d.setStroke(new BasicStroke(1, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
      if (nearestIdx >= 0 && nearestIdx < selectedConformations.size) {
        val c = selectedConformations(nearestIdx)
        val x = c.x.toInt
        val y = c.y.toInt
        g2d.setColor(Color.orange)
        val oval = new Ellipse2D.Double(x - 5, y - 5, 9, 9)
        g2d.draw(oval)
      }

      val pad = 8
      val axisLength = 50
      g2d.setColor(new Color(73, 96, 112, 120).brighter)
      g2d.drawLine(pad, bounds.height - pad, pad + axisLength, bounds.height - pad)
      g2d.drawLine(pad + axisLength, bounds.height - pad, pad + axisLength - 5, bounds.height - pad - 5)
      g2d.drawLine(pad + axisLength, bounds.height - pad, pad + axisLength - 5, bounds.height - pad + 5)

      g2d.drawLine(pad, bounds.height - pad, pad, bounds.height - axisLength - pad)
      g2d.drawLine(pad, bounds.height - axisLength - pad, pad - 5, bounds.height - axisLength - pad + 5)
      g2d.drawLine(pad, bounds.height - axisLength - pad, pad + 5, bounds.height - axisLength - pad + 5)

      g2d.setColor(ColorSchemes.scheme.bgColor)
      g2d.drawString("Conformation Coordinate", 2 * pad + axisLength, bounds.height - 3)

      val oldTrans = g2d.getTransform()

      g2d.translate(pad + 3, bounds.height - 2 * pad - axisLength)
      g2d.rotate(-math.Pi / 2.0)
      g2d.drawString("Function Value", 0, 0)

      g2d.setTransform(oldTrans)
      val border = new Rectangle2D.Double(0, -1, size.width - 1, size.height - 1)
      g2d.setColor(new Color(40, 40, 40))
      g2d.draw(border)
    }

  }
}

case class ConformationPointClicked(conformationID: Int, pdbLines: Array[String]) extends Event

object SecondaryStructureColors {
  val ssColorMapOld = Map(
    "structure=-1" -> new Color(180, 180, 180), // non-protein parts (ligands)
    "structure=0" -> new Color(60, 60, 60), // unstructure protein (random coil)
    "helixalpha" -> new Color(166, 206, 227),
    "helix310" -> new Color(31, 120, 180),
    "helixpi" -> new Color(178, 223, 138),
    "sheet" -> new Color(51, 160, 44),
    "turn" -> new Color(251, 154, 153))
}
