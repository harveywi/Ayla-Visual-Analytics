package edu.osu.compgeom.ayla.menu

import scala.swing._
import scala.swing.event._
import java.io._
import java.awt.Color
import java.awt.Shape
import java.awt.geom._
import edu.osu.compgeom.util.IO._
import edu.osu.compgeom.omegavis.RingMenu
import org.jgrapht.graph.{ DefaultEdge, SimpleDirectedGraph }
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import java.awt.RenderingHints
import java.awt.image.BufferedImage
import org.artemis.progx.graphics.GraphicsUtilities
import org.artemis.progx.image.ColorTintFilter
import org.artemis.progx.image.GaussianBlurFilter
import java.awt.font.TextLayout
import java.awt.BasicStroke
import javax.swing.OverlayLayout
import javax.swing.JLayeredPane
import java.net.URL

@SerialVersionUID(1L)
class PieMenu(graph: SimpleDirectedGraph[PieMenuItem, DefaultEdge]) extends Component with Serializable {
  var selectedItem: Option[PieMenuItem] = None
  
  var mainFrame: Window = null
  val menuDimensions = new Dimension(500, 500)
  listenTo(this)
  var curRoot: PieMenuItem = PieMenuRoot

  val backdrop: BufferedImage = {
    val url = Thread.currentThread().getContextClassLoader().getResource("ring.png")
    var bgImage = GraphicsUtilities.loadCompatibleImage(url)
    bgImage = new ColorTintFilter(Color.BLACK, 0.10f).filter(bgImage, null);
    bgImage
  }
  opaque = true

  listenTo(mouse.clicks)
  listenTo(mouse.moves)

  override def visible_=(visible: Boolean): Unit = {
    if (visible) {
      curRoot = PieMenuRoot
      initAreas
      //      backdrop = GraphicsUtilities.createCompatibleImage(mainFrame.peer.getRootPane().getWidth(),
      //        mainFrame.peer.getRootPane().getHeight());
      //      val g2 = backdrop.createGraphics();
      //      mainFrame.peer.getRootPane().paint(g2);
      //      g2.dispose();
      //
      //      backdrop = GraphicsUtilities.createThumbnail(backdrop, mainFrame.peer.getRootPane().getWidth() / 2);
      //      backdrop = new ColorTintFilter(Color.BLACK, 0.10f).filter(backdrop, null);
      //      backdrop = new GaussianBlurFilter(2).filter(backdrop, null)
    } else {
      //      if (backdrop != null)
      //        backdrop.flush()
      //      backdrop = null
    }
    super.visible_=(visible)
  }

  override def paintComponent(g2d: Graphics2D): Unit = {
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
    //g2d.drawImage(backdrop, bounds.x, bounds.y, bounds.width, bounds.height, null);
    //    g2d.drawImage(backdrop, bounds.width / 2 - backdrop.getWidth() / 2, bounds.height / 2 - backdrop.getHeight() / 2, null)
    g2d.setColor(Color.black)
    g2d.fillRect(0, 0, bounds.width, bounds.height)
    if (bounds.width < bounds.height) {
      g2d.drawImage(backdrop, bounds.x, bounds.y + (bounds.height - bounds.width) / 2, bounds.width, bounds.width, null)
    } else {
      g2d.drawImage(backdrop, bounds.x + (bounds.width - bounds.height) / 2, bounds.y, bounds.height, bounds.height, null)
    }

    val cx = bounds.x + bounds.width / 2
    val cy = bounds.y + bounds.height / 2
    g2d.setColor(Color.red)
    val r = 200

    def drawString(s: String, cx: Double, cy: Double): Unit = {
      val fontMetrics = g2d.getFontMetrics
      val layout = new TextLayout(s, g2d.getFont, g2d.getFontRenderContext)

      val x = cx - layout.getBounds().getWidth / 2.0
      val y = cy + layout.getBounds().getHeight / 2.0
      g2d.drawString(s, x.toFloat, y.toFloat)
    }

    g2d.translate(cx - menuDimensions.width / 2, cy - menuDimensions.height / 2)

    menuAreas.foreach(area => {
      if (area.menuItem.text != "Cancel" && graph.outDegreeOf(area.menuItem) > 0)
        g2d.setColor(new Color(80, 80, 153, 220))
      else
        g2d.setColor(new Color(54, 54, 153, 180))

      g2d.fill(area)
      val oldStroke = g2d.getStroke
      if (area.highlighted)
        g2d.setStroke(new BasicStroke(10))
      g2d.setColor(new Color(10, 10, 153, 200))
      g2d.draw(area)
      g2d.setStroke(oldStroke)
      g2d.setColor(new Color(220, 220, 255))
      val areaBounds = area.getBounds

      val t = g2d.getTransform()
      g2d.rotate(if (math.cos(area.cAngle) < 0) area.cAngle + math.Pi else area.cAngle, area.center.x, area.center.y)
      drawString(area.menuItem.text, area.center.x, area.center.y)
      g2d.setTransform(t)
    })
  }

  def mouseToMenu(p: Point): Option[(Double, Double)] = {
    val x = p.x - bounds.getCenterX + menuDimensions.width / 2
    val y = p.y - bounds.getCenterY + menuDimensions.height / 2
    if (x >= 0 && y >= 0 && x <= menuDimensions.width && y <= menuDimensions.height) {
      Some((x, y))
    } else {
      None
    }
  }

  def repaintDirtyMenu() = {
    val pad = 10
    val dirtyArea = new Rectangle(bounds.getCenterX.toInt - menuDimensions.width / 2 - pad, bounds.getCenterY.toInt - menuDimensions.height / 2 - pad,
      menuDimensions.width + 2 * pad, menuDimensions.height + 2 * pad)
    repaint(dirtyArea)
  }

  reactions += {
    case e: UIElementResized => {
      initAreas()
      repaint()
    }
    case e: MouseClicked => {
      mouseToMenu(e.point) match {
        case Some((x, y)) => {
          menuAreas.find(_.contains(x, y)) match {
            case Some(area) => {
              if (area.menuItem.text == "Cancel") {
                // Try to go back up the menu
                if (curRoot.text == "__root__") {
                  visible = false
                } else {
                  val parent = graph.getEdgeSource(graph.incomingEdgesOf(curRoot).iterator().next)
                  curRoot = parent
                  initAreas()
                  repaintDirtyMenu()
                }
              } else if (graph.outDegreeOf(area.menuItem) == 0) {
                selectedItem = Some(area.menuItem)
                visible = false
              } else {
                // Pull up the sub-menu
                curRoot = area.menuItem
                //								area.menuItem.handleClick()
                initAreas()
                repaintDirtyMenu()
              }
            }
            case None => { visible = false }
          }
        }
        case None => { visible = false }
      }
    }
    case e: MouseEntered => {
      menuAreas.foreach(_.highlighted = false)
      repaintDirtyMenu()
    }
    case e: MouseExited => {
      menuAreas.foreach(_.highlighted = false)
      repaintDirtyMenu()
    }
    case e: MouseMoved => {
      mouseToMenu(e.point) match {
        case Some((x, y)) => {
          menuAreas.foreach(area => {
            if (area.getBounds().contains(x, y) && area.contains(x, y)) {
              if (!area.highlighted) {
                area.highlighted = true
                repaintDirtyMenu()
              }
            } else {
              if (area.highlighted) {
                area.highlighted = false
                repaintDirtyMenu()
              }
            }
          })
        }
        case None => { menuAreas.foreach(_.highlighted = false) }
      }
    }
  }

  val menuAreas = new ArrayBuffer[PieMenuArea]

  def initAreas(): Unit = {
    menuAreas.clear()
    val brect = new Rectangle(0, 0, menuDimensions.width, menuDimensions.height)
    val cx = brect.x + brect.width / 2
    val cy = brect.y + brect.height / 2

    val cancelRad = 30
    val cancelArea = new PieMenuArea(new Ellipse2D.Double(cx - cancelRad, cy - cancelRad, 2 * cancelRad, 2 * cancelRad), new PieMenuItem("Cancel"), new Point2D.Double(cx, cy), 0)

    val numChildren = graph.outgoingEdgesOf(curRoot).size
    val angleGap = math.toRadians(10) // radians
    val centerGap = 10 // pixels
    val rectInner = new Rectangle2D.Double(cx - cancelRad - centerGap, cy - cancelRad - centerGap, 2 * (cancelRad + centerGap), 2 * (cancelRad + centerGap))
    val rectOuter = new Rectangle2D.Double(0, 0, menuDimensions.width, menuDimensions.height)

    // Each child will subtend this angle
    val subtendAngle = (2.0 * math.Pi) / numChildren.toDouble
    val childAreas = graph.outgoingEdgesOf(curRoot).asScala.map(e => graph.getEdgeTarget(e)).toIndexedSeq.sortBy[String](_.text)(new WindowsExplorerStringOrdering).reverse.zipWithIndex.map {
      case (child, i) => {
        val theta1 = i * subtendAngle + angleGap / 2.0 + math.Pi / 2.0
        val theta2 = theta1 + subtendAngle - angleGap
        val theta1Deg = math.toDegrees(theta1)
        val theta2Deg = math.toDegrees(theta2)

        val s = rectOuter.width / rectInner.width.toDouble
        val theta3Deg = math.toDegrees(i * subtendAngle + (angleGap / s) / 2.0 + math.Pi / 2.0)
        val theta4Deg = theta3Deg + math.toDegrees(subtendAngle - angleGap / s)

        val gp = new GeneralPath
        gp.append(new Arc2D.Double(rectInner.x, rectInner.y, rectInner.width, rectInner.height, theta1Deg, theta2Deg - theta1Deg, Arc2D.OPEN), true)
        gp.append(new Arc2D.Double(rectOuter.x, rectOuter.y, rectOuter.width, rectOuter.height, theta4Deg, theta3Deg - theta4Deg, Arc2D.OPEN), true)
        gp.closePath

        // Compute the center of this area (for displaying text)
        val cAngle = -(theta1 + theta2) / 2.0 // Draw progress bar

        val pInner = new Point2D.Double(cx + cancelRad * math.cos(cAngle), cy + cancelRad * math.sin(cAngle))
        val outerRad = menuDimensions.width / 2.0
        val pOuter = new Point2D.Double(cx + outerRad * math.cos(cAngle), cy + outerRad * math.sin(cAngle))
        val center = new Point2D.Double((pInner.x + pOuter.x) / 2.0, (pInner.y + pOuter.y) / 2.0)

        new PieMenuArea(new Area(gp), child, center, cAngle)
      }
    }

    menuAreas += cancelArea
    menuAreas ++= childAreas
  }

  @SerialVersionUID(1L)
  class PieMenuArea(shape: Shape, val menuItem: PieMenuItem, val center: Point2D.Double, val cAngle: Double) extends Area(shape)
    with Serializable {
    var highlighted = false
//    var highlighted0: Boolean = false
//    def highlighted = highlighted0
//    def highlighted_=(h: Boolean) = {
//      highlighted0 = h
////      if (highlighted0)
////        menuItem.publish(PieMenuItemHighlighted(menuItem))
//    }
  }

}

case object PieMenuExited extends Event

@SerialVersionUID(1L)
case class PieMenuItemHighlighted(val menuItem: PieMenuItem) extends Event with Serializable

@SerialVersionUID(1L)
class PieMenuItem(val text: String, val userData: Option[Any] = None) extends Serializable {
  var enabled = true

  override def toString = text
}

object PieMenuRoot extends PieMenuItem("__root__")

object PieMenu {
  type PieMenuGraph = SimpleDirectedGraph[PieMenuItem, DefaultEdge]
  def newGraph() = {
    val g = new PieMenuGraph(classOf[DefaultEdge])
    g.addVertex(PieMenuRoot)
    g
  }
}

object PieMenuTest extends SimpleSwingApplication {

  val projDir = new File("/home/harveywi/research/ScalaCrystals/Survivin_252996")
  val sfDir = new File(projDir, "scalar_functions")

  val g = PieMenu.newGraph()

  val stack = new scala.collection.mutable.Stack[PieMenuItem]
  stack.push(PieMenuRoot)
  while (!stack.isEmpty) {
    val n = stack.pop
    val childNodes = (n.userData match {
      case Some(file: File) => {
        file.listFiles.map {
          case f if f.isDirectory => new PieMenuItem(f.getName, Some(f))
          case f if f.getName.endsWith(".txt") => {
            val name = withBufferedReader(f) { _.readLine() }
            new PieMenuItem(name, Some(f))
          }
        }
      }
      case None => sfDir.listFiles.filter(_.isDirectory).map(f => new PieMenuItem(f.getName, Some(f)))
    }).sortBy[String](_.text)(new WindowsExplorerStringOrdering)

    val maxGroupSize = 30
    if (childNodes.size > maxGroupSize) {
      childNodes.grouped(maxGroupSize).foreach { group =>
        val dummyName = group.head.text + " - " + group.last.text
        val dummyParent = new PieMenuItem(dummyName, None)
        g.addVertex(dummyParent)
        g.addEdge(n, dummyParent)
        group.foreach { child =>
          g.addVertex(child)
          g.addEdge(dummyParent, child)
          child.userData match {
            case Some(f: File) if f.isDirectory => stack.push(child)
            case _ => {}
          }
        }
      }
    } else {
      childNodes.foreach { child =>
        g.addVertex(child)
        g.addEdge(n, child)
        child.userData match {
          case Some(f: File) if f.isDirectory => stack.push(child)
          case _ => {}
        }
      }
    }
  }

  println(g.toString())

  val pm = new PieMenu(g)
  def top = new MainFrame {
    pm.mainFrame = this
    title = "Hello world"
    contents = new FlowPanel {
      background = Color.black
      opaque = true
      preferredSize = new Dimension(500, 600)
    }
    val layeredPane = peer.getRootPane.getLayeredPane
    layeredPane.setLayout(new OverlayLayout(layeredPane))
    layeredPane.add(pm.peer, JLayeredPane.MODAL_LAYER)
    pm.visible = true
  }
}

@SerialVersionUID(1L)
class WindowsExplorerStringOrdering extends Ordering[String] with Serializable {
  private var str1: String = null
  private var str2: String = null
  private var pos1: Int = 0
  private var pos2: Int = 0
  private var len1: Int = 0
  private var len2: Int = 0

  def compare(s1: String, s2: String): Int =
    {
      str1 = s1;
      str2 = s2;
      len1 = str1.length();
      len2 = str2.length();
      pos1 = 0
      pos2 = 0

      var result = 0;
      while (result == 0 && pos1 < len1 && pos2 < len2) {
        val ch1 = str1.charAt(pos1);
        val ch2 = str2.charAt(pos2);

        if (Character.isDigit(ch1)) {
          result = if (Character.isDigit(ch2)) compareNumbers() else -1;
        } else if (Character.isLetter(ch1)) {
          result = if (Character.isLetter(ch2)) compareOther(true) else 1;
        } else {
          result = if (Character.isDigit(ch2)) 1
          else if (Character.isLetter(ch2)) -1
          else compareOther(false);
        }

        pos1 += 1;
        pos2 += 1;
      }

      return if (result == 0) len1 - len2 else result;
    }

  private def compareNumbers(): Int =
    {
      var end1 = pos1 + 1;
      while (end1 < len1 && Character.isDigit(str1.charAt(end1))) {
        end1 += 1;
      }
      val fullLen1 = end1 - pos1;
      while (pos1 < end1 && str1.charAt(pos1) == '0') {
        pos1 += 1;
      }

      var end2 = pos2 + 1;
      while (end2 < len2 && Character.isDigit(str2.charAt(end2))) {
        end2 += 1;
      }
      val fullLen2 = end2 - pos2;
      while (pos2 < end2 && str2.charAt(pos2) == '0') {
        pos2 += 1;
      }

      var delta = (end1 - pos1) - (end2 - pos2);
      if (delta != 0) {
        return delta;
      }

      while (pos1 < end1 && pos2 < end2) {
        delta = str1.charAt(pos1) - str2.charAt(pos2);
        pos1 += 1
        pos2 += 1
        if (delta != 0) {
          return delta;
        }
      }

      pos1 -= 1;
      pos2 -= 1;

      return fullLen2 - fullLen1;
    }

  private def compareOther(isLetters: Boolean): Int =
    {
      var ch1 = str1.charAt(pos1);
      var ch2 = str2.charAt(pos2);

      if (ch1 == ch2) {
        return 0;
      }

      if (isLetters) {
        ch1 = Character.toUpperCase(ch1);
        ch2 = Character.toUpperCase(ch2);
        if (ch1 != ch2) {
          ch1 = Character.toLowerCase(ch1);
          ch2 = Character.toLowerCase(ch2);
        }
      }

      return ch1 - ch2;
    }
}