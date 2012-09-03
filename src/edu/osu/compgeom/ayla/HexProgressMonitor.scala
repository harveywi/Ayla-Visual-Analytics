package edu.osu.compgeom.ayla

import scala.swing._
import scala.swing.event._
import java.awt.Color
import java.awt.Rectangle
import java.awt.geom._
import java.awt.geom.Line2D.{ Double => Line }
import java.awt.geom.Point2D.{ Double => Pt }
import java.awt.BasicStroke
import no.uib.cipr.matrix.DenseMatrix
import javax.vecmath.Vector2d
import javax.swing.OverlayLayout
import javax.swing.SwingUtilities
import java.awt.RenderingHints
import javax.swing.WindowConstants
import javax.swing.JOptionPane
import java.io.{ StringWriter, PrintWriter }

import akka.actor._
import akka.dispatch.ExecutionContext

private[ayla] trait HexProgressMonitor2 extends Dialog with DataflowProgressMonitor {
  val indeterminate: Boolean = false

  contents = new Component {
    private[this] val segs = HexProgressMonitor2.makeSegs(3)

    preferredSize = new Dimension(500, 500)
    private[this] val cellBounds = new Rectangle(0, 0, 500, 500) {
      val allPts = segs.map(_.p) ++ segs.flatMap(_.hexVerts)
      val minX = allPts.minBy(_.x).x
      val minY = allPts.minBy(_.y).y
      val maxX = allPts.maxBy(_.x).x
      val maxY = allPts.maxBy(_.y).y
      val scale = 500 / (maxX - minX)
      allPts.foreach { p =>
        val newX = (p.x - minX) * scale
        val newY = (p.y - minY) * scale
        p.setLocation(newX, newY)
      }
      segs.foreach { s =>
        s.v1.scale(scale)
        s.v2.scale(scale)
      }
    }

    private[this] var indeterminateAlpha = 0f
    private[this] val indeterminateAlphaTimer = new javax.swing.Timer(0, new java.awt.event.ActionListener() {
      val increaseAmt = .02f
      override def actionPerformed(e: java.awt.event.ActionEvent): Unit = {
        indeterminateAlpha += increaseAmt
        if (indeterminateAlpha > 1f)
          indeterminateAlpha -= 1f
        repaint()
      }
    })
    indeterminateAlphaTimer.setDelay(50)
    indeterminateAlphaTimer.start

    override def paintComponent(g2d: Graphics2D) {
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2d.setColor(Color.black)
      g2d.fillRect(0, 0, bounds.width, bounds.height)

      def getStringDims(g2d: Graphics2D, s: String): java.awt.Dimension = {
        val fontMetrics = g2d.getFontMetrics
        val layout = new java.awt.font.TextLayout(s, g2d.getFont, g2d.getFontRenderContext)
        val bounds = layout.getBounds
        new Dimension(bounds.getWidth.toInt, bounds.getHeight.toInt)
      }

      segs.zipWithIndex.foreach {
        case (s, idx) =>
          val p1x = s.p.getX
          val p1y = s.p.getY
          val p2x = (s.p.getX + s.v1.x)
          val p2y = (s.p.getY + s.v1.y)
          val p3x = (s.p.getX + s.v2.x)
          val p3y = (s.p.getY + s.v2.y)

          val alpha = (idx + 1) / segs.size.toDouble
          val color = if (idx / segs.size.toDouble < progress)
            new Color((alpha * 255).toInt, (alpha * 255).toInt, 255)
          else
            new Color((alpha * 255).toInt, (alpha * 255).toInt, 255)
          g2d.setColor(color)

          val path = new Path2D.Double
          path.moveTo(s.hexVerts(0).getX, s.hexVerts(0).getY)
          s.hexVerts.foreach(v => path.lineTo(v.getX, v.getY))
          path.closePath

          val area = new Area(path)
          g2d.setColor(new Color(color.getRed, color.getGreen, color.getBlue, 100))

          if (indeterminate) {
            val exps = List(math.abs(alpha - indeterminateAlpha), math.abs(1 + alpha - indeterminateAlpha), math.abs(alpha - indeterminateAlpha - 1))
            val shade = math.exp(-exps.min) //math.max(math.exp(-math.abs(alpha - indeterminateAlpha)), math.exp(-math.abs(alpha - (indeterminateAlpha-1))))
            g2d.setColor(new Color((shade * 180).toInt, (shade * 180).toInt, 255))
            g2d.fill(area)
          } else {
            if (idx / segs.size.toDouble < progress) {
              val exps = List(math.abs(alpha - indeterminateAlpha), math.abs(1 + alpha - indeterminateAlpha), math.abs(alpha - indeterminateAlpha - 1))
              val shade = math.exp(-exps.min) //math.max(math.exp(-math.abs(alpha - indeterminateAlpha)), math.exp(-math.abs(alpha - (indeterminateAlpha-1))))
              g2d.setColor(new Color((shade * 180).toInt, (shade * 180).toInt, 255))
              g2d.fill(area)
            } else
              g2d.draw(area)
          }
      }

      var y = cellBounds.height
      val numVisibleStrings = 30
      statusMessages.reverseIterator.zipWithIndex.foreach {
        case (s, idx) =>
          val dims = getStringDims(g2d, s)
          y -= dims.height + 3
          val x = (cellBounds.width - dims.width) / 2
          val alpha = (math.max(0, (numVisibleStrings - idx) / numVisibleStrings.toDouble) * 255).toInt
          g2d.setColor(new Color(200, 200, 255, alpha))
          g2d.drawString(s, x, y)
      }
    }
  }

  pack()

  var progress: Double = 0
  var statusMessages: Array[String] = Array.empty

  def onProgressUpdate(prog: Double, statusMessages: Array[String]) = {
    Swing.onEDT {
      progress = prog
      this.statusMessages = statusMessages
    }
  }

  override def onStart = {
    Swing.onEDT {
      open()
    }
  }

  override def onDone = {
    Swing.onEDT {
      close()
    }
  }
}

object HexProgressMonitor2 {
  def runTasks[T](title2: String, numStatusMessagesToShow: Int = 20,
    isIndeterminate: Boolean = false)(op: HexProgressMonitor2 => T): T = {
    val progMon = apply(title2, numStatusMessagesToShow, isIndeterminate)
    progMon.onStart
    val result = op(progMon)
    result
  }

  private def apply(title2: String, numStatusMessagesToShow: Int = 20, isIndeterminate: Boolean = false): HexProgressMonitor2 = new {
    override val system = ActorSystem("HexProgressMonitor")
    override val numStatusMessages = numStatusMessagesToShow
    override val indeterminate: Boolean = isIndeterminate
    override val ec = ExecutionContext.defaultExecutionContext(system)
  } with HexProgressMonitor2 {
    title = title2
  }

  protected case class GosperSegment(val p: Pt, val v1: Vector2d, val v2: Vector2d, val idx: Int) {
    val hexVerts = {
      val A = new DenseMatrix(2, 2)
      A.set(0, 0, v1.x)
      A.set(1, 0, v1.y)
      A.set(0, 1, v2.x)
      A.set(1, 1, v2.y)

      val Y = new DenseMatrix(2, 6)
      A.mult(C, Y)

      (0 until 6).map { i =>
        new Pt(p.getX + Y.get(0, i), p.getY + Y.get(1, i))
      }.toArray
    }
  }

  private val B = {
    val pts = Array(
      new Pt(0, 0),
      new Pt(1, 0),
      new Pt(1.5, math.sqrt(3) / 2.0),
      new Pt(.5, math.sqrt(3) / 2.0),
      new Pt(0, math.sqrt(3)),
      new Pt(1, math.sqrt(3)),
      new Pt(2, math.sqrt(3)),
      new Pt(2.5, math.sqrt(3) / 2.0))

    val base1 = pts(7)
    val base2 = new Pt(pts(7).getY, -pts(7).getX)

    val A = new DenseMatrix(2, 2)
    A.set(0, 0, base1.getX)
    A.set(1, 0, base1.getY)
    A.set(0, 1, base2.getX)
    A.set(1, 1, base2.getY)

    val Y = new DenseMatrix(2, pts.size)
    pts.indices.foreach { i =>
      Y.set(0, i, pts(i).getX)
      Y.set(1, i, pts(i).getY)
    }
    val B = new DenseMatrix(2, pts.size)
    A.solve(Y, B)
    B
  }

  // Hexagon coefficients
  private val C = {
    val pts = (0 until 6).map { i =>
      val theta = 2 * math.Pi * i / 6.0
      new Pt(math.cos(theta), math.sin(theta))
    }
    val base1 = new Vector2d(pts(2).getX - pts(0).getX, pts(2).getY - pts(0).getY)
    val base2 = new Pt(base1.y, -base1.x)

    val A = new DenseMatrix(2, 2)
    A.set(0, 0, base1.x)
    A.set(1, 0, base1.y)
    A.set(0, 1, base2.x)
    A.set(1, 1, base2.y)

    val Y = new DenseMatrix(2, pts.size)
    pts.indices.foreach { i =>
      Y.set(0, i, pts(i).getX - pts(0).getX)
      Y.set(1, i, pts(i).getY - pts(0).getY)
    }
    val C = new DenseMatrix(2, pts.size)
    A.solve(Y, C)
    C
  }

  private[this] val segPairs = Array(
    (0, 1),
    (2, 1),
    (3, 2),
    (3, 4),
    (4, 5),
    (5, 6),
    (7, 6))

  def makeSegs(maxDepth: Int): Array[GosperSegment] = {
    val initialSegment = GosperSegment(new Pt(0, 0), new Vector2d(2.5, math.sqrt(3) / 2.0), new Vector2d(math.sqrt(3) / 2.0, -2.5), 0)
    val queue = new scala.collection.mutable.Queue[(GosperSegment, Int, Boolean)]
    queue.enqueue((initialSegment, 0, false))
    val ret = new scala.collection.mutable.ArrayBuffer[GosperSegment]
    while (!queue.isEmpty) {
      val (seg, depth, reverseOrder) = queue.dequeue()
      if (depth == maxDepth) {
        ret += seg
      } else {
        // Subdivide the segment
        val A = new DenseMatrix(2, 2)
        A.set(0, 0, seg.v1.x)
        A.set(1, 0, seg.v1.y)
        A.set(0, 1, seg.v2.x)
        A.set(1, 1, seg.v2.y)

        val Y = new DenseMatrix(2, 8)
        A.mult(B, Y)

        (if (reverseOrder) segPairs.reverse else segPairs).foreach {
          case (i, j) => {
            val p1 = new Pt(seg.p.getX + Y.get(0, i), seg.p.getY + Y.get(1, i))
            val p2 = new Pt(seg.p.getX + Y.get(0, j), seg.p.getY + Y.get(1, j))

            val v1 = new Vector2d(p2.x - p1.x, p2.y - p1.y)
            val v2 = new Vector2d(v1.y, -v1.x)
            val newSeg = new GosperSegment(p1, v1, v2, 32)
            queue.enqueue((newSeg, depth + 1, (i > j) ^ reverseOrder))
          }
        }
      }
    }
    return ret.toArray
  }
}

//@deprecated("Use HexProgressMonitor2 instead", "0.1")
//object HexProgressMonitor {
//  def main(args: Array[String]): Unit = {
//    val result = runTask("Reticulating splines", 1000) { pt =>
//      (0 to 1000 by 3).map { i =>
//        Thread.sleep((math.random * 10).toLong)
//        pt.update(i, "Processing " + i)
//        i
//      }.sum
//    }
//
//    println("Result:  " + result)
//    System.exit(0)
//  }
//
//  def runTaskIndeterminate[T](title: String)(task: ProgressTracker => T): T = runTask(title, 0, true)(task)
//  def runTask[T](title: String, taskCount: Int)(task: ProgressTracker => T): T = runTask(title, taskCount, false)(task)
//
//  private[this] def runTask[T](titlebarText: String, taskCount: Int, indeterminate: Boolean)(task: ProgressTracker => T): T = {
//    abstract class HexDialog[T] extends Dialog {
//      title = titlebarText
//      val hexProgMon: HexProgressMonitor[T]
//    }
//
//    case object Open extends Event
//    case object Close extends Event
//
//    val dlg = new HexDialog[T] {
//      var myResult: Option[T] = None
//      listenTo(this)
//      peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
//      val hexProgMon = new HexProgressMonitor[T](taskCount, indeterminate)
//      modal = true
//      contents = hexProgMon
//      resizable = false
//      reactions += {
//        case e: WindowClosing => {
//          Dialog.showConfirmation(Component.wrap(contents(0).peer), "Exit Ayla Client?") match {
//            case Dialog.Result.Ok => System.exit(0)
//            case _ => { /*Do nothing*/ }
//          }
//          println("closing")
//        }
//        case Open => open()
//        case Close => close()
//      }
//      centerOnScreen()
//    }
//
//    val p = new Publisher {
//      var result: Option[T] = None
//      scala.actors.Actor.actor {
//        dlg.listenTo(this)
//        result = Some(task(dlg.hexProgMon.progTracker))
//        publish(Close)
//      }
//    }
//
//    dlg.open()
//
//    return p.result.get
//  }
//
//  protected case class GosperSegment(val p: Pt, val v1: Vector2d, val v2: Vector2d, val idx: Int) {
//    val hexVerts = {
//      val A = new DenseMatrix(2, 2)
//      A.set(0, 0, v1.x)
//      A.set(1, 0, v1.y)
//      A.set(0, 1, v2.x)
//      A.set(1, 1, v2.y)
//
//      val Y = new DenseMatrix(2, 6)
//      A.mult(C, Y)
//
//      (0 until 6).map { i =>
//        new Pt(p.getX + Y.get(0, i), p.getY + Y.get(1, i))
//      }.toArray
//    }
//  }
//
//  private val B = {
//    val pts = Array(
//      new Pt(0, 0),
//      new Pt(1, 0),
//      new Pt(1.5, math.sqrt(3) / 2.0),
//      new Pt(.5, math.sqrt(3) / 2.0),
//      new Pt(0, math.sqrt(3)),
//      new Pt(1, math.sqrt(3)),
//      new Pt(2, math.sqrt(3)),
//      new Pt(2.5, math.sqrt(3) / 2.0))
//
//    val base1 = pts(7)
//    val base2 = new Pt(pts(7).getY, -pts(7).getX)
//
//    val A = new DenseMatrix(2, 2)
//    A.set(0, 0, base1.getX)
//    A.set(1, 0, base1.getY)
//    A.set(0, 1, base2.getX)
//    A.set(1, 1, base2.getY)
//
//    val Y = new DenseMatrix(2, pts.size)
//    pts.indices.foreach { i =>
//      Y.set(0, i, pts(i).getX)
//      Y.set(1, i, pts(i).getY)
//    }
//    val B = new DenseMatrix(2, pts.size)
//    A.solve(Y, B)
//    B
//  }
//
//  // Hexagon coefficients
//  private val C = {
//    val pts = (0 until 6).map { i =>
//      val theta = 2 * math.Pi * i / 6.0
//      new Pt(math.cos(theta), math.sin(theta))
//    }
//    val base1 = new Vector2d(pts(2).getX - pts(0).getX, pts(2).getY - pts(0).getY)
//    val base2 = new Pt(base1.y, -base1.x)
//
//    val A = new DenseMatrix(2, 2)
//    A.set(0, 0, base1.x)
//    A.set(1, 0, base1.y)
//    A.set(0, 1, base2.x)
//    A.set(1, 1, base2.y)
//
//    val Y = new DenseMatrix(2, pts.size)
//    pts.indices.foreach { i =>
//      Y.set(0, i, pts(i).getX - pts(0).getX)
//      Y.set(1, i, pts(i).getY - pts(0).getY)
//    }
//    val C = new DenseMatrix(2, pts.size)
//    A.solve(Y, C)
//    C
//  }
//
//  private[this] val segPairs = Array(
//    (0, 1),
//    (2, 1),
//    (3, 2),
//    (3, 4),
//    (4, 5),
//    (5, 6),
//    (7, 6))
//
//  def makeSegs(maxDepth: Int): Array[GosperSegment] = {
//    val initialSegment = GosperSegment(new Pt(0, 0), new Vector2d(2.5, math.sqrt(3) / 2.0), new Vector2d(math.sqrt(3) / 2.0, -2.5), 0)
//    val queue = new scala.collection.mutable.Queue[(GosperSegment, Int, Boolean)]
//    queue.enqueue((initialSegment, 0, false))
//    val ret = new scala.collection.mutable.ArrayBuffer[GosperSegment]
//    while (!queue.isEmpty) {
//      val (seg, depth, reverseOrder) = queue.dequeue()
//      if (depth == maxDepth) {
//        ret += seg
//      } else {
//        // Subdivide the segment
//        val A = new DenseMatrix(2, 2)
//        A.set(0, 0, seg.v1.x)
//        A.set(1, 0, seg.v1.y)
//        A.set(0, 1, seg.v2.x)
//        A.set(1, 1, seg.v2.y)
//
//        val Y = new DenseMatrix(2, 8)
//        A.mult(B, Y)
//
//        (if (reverseOrder) segPairs.reverse else segPairs).foreach {
//          case (i, j) => {
//            val p1 = new Pt(seg.p.getX + Y.get(0, i), seg.p.getY + Y.get(1, i))
//            val p2 = new Pt(seg.p.getX + Y.get(0, j), seg.p.getY + Y.get(1, j))
//
//            val v1 = new Vector2d(p2.x - p1.x, p2.y - p1.y)
//            val v2 = new Vector2d(v1.y, -v1.x)
//            val newSeg = new GosperSegment(p1, v1, v2, 32)
//            queue.enqueue((newSeg, depth + 1, (i > j) ^ reverseOrder))
//          }
//        }
//      }
//    }
//    return ret.toArray
//  }
//}