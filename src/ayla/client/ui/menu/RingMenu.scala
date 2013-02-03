/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui.menu

import java.awt.BasicStroke
import java.awt.font.TextLayout
import org.artemis.progx.image.GaussianBlurFilter
import org.artemis.progx.image.ColorTintFilter
import java.awt.image.BufferedImage
import scala.swing._
import scala.swing.event._
import scala.collection.JavaConverters._
import javax.swing._
import java.awt.{Point, Graphics, Color, RenderingHints, Shape}
import org.artemis.progx.graphics._
import org.jgrapht.graph.{SimpleDirectedGraph, DefaultEdge}
import java.awt.geom._
import ayla.client.ui.event.RingMenuUpdate
import java.awt.AlphaComposite

abstract class RingMenuProgressListener {
  def postProgress(text: String, newProgress: Float)
}

class RingMenu(mainFrame: Frame) extends Component {
	
	class MenuArea(shape: Shape, val menuItem: RingMenuItem, val center: Point2D.Double, val cAngle: Double) extends Area(shape) {
		var highlighted = false
	}
	
	class ProgressArea(shape: Shape, val progress: Float) extends Area(shape)
	
	val menuDimensions = new Dimension(500, 500)

	var curRoot: RingMenuItem = null
	
	// menuGraph has a custom setter that automatically resets the root node.
	private[this] var _menuGraph: SimpleDirectedGraph[RingMenuItem, DefaultEdge] = null
	def menuGraph = _menuGraph
	def menuGraph_= (value: SimpleDirectedGraph[RingMenuItem, DefaultEdge]): Unit = {
	  javax.swing.SwingUtilities.invokeLater(new Runnable(){def run = {
	  _menuGraph = value
	  curRoot = getDefaultRoot	    
	  }})
	} 
	
	def getDefaultRoot = menuGraph.vertexSet().asScala.find(item => item.text == "Root").get
	
	opaque = true
	listenTo(mouse.clicks)
	listenTo(mouse.moves)
	var backdrop: BufferedImage = null
	
	var working = false
	var taskText: String = ""
	var taskProgress = 0f

	var alpha: Float = 0f
	val indeterminateAlphaTimer = new javax.swing.Timer(0, new java.awt.event.ActionListener(){
	  var increaseAlpha = true
	  val increaseAmt = .15f
	  override def actionPerformed(e: java.awt.event.ActionEvent): Unit = {
	    if (increaseAlpha) {
	      alpha += increaseAmt
	      if (alpha > 1f) {
	        alpha = 1f
	        increaseAlpha = false
	      }
	    } else {
	      alpha -= increaseAmt
	      if (alpha < 0) {
	        alpha = 0f
	        increaseAlpha = true
	      }
	    }
//	    repaint()
	  }
	})
	indeterminateAlphaTimer.setDelay(100)

//	var okRegion: Area = null
//	var cancelRegion: Area = null
//	
//	var wedge: Area = null
	
	override def paintComponent(g2d: Graphics2D): Unit = {
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
		g2d.drawImage(backdrop, bounds.x, bounds.y, bounds.width, bounds.height, null);
		
//		g2d.setColor(new Color(0, 0, 0, 40))
//		g2d.fillRect(bounds.x, bounds.y, bounds.width, bounds.height)
		
		val cx = bounds.x + bounds.width / 2
		val cy = bounds.y + bounds.height / 2
		g2d.setColor(Color.red)
		val r = 200
//		g2d.fillOval(cx - r, cy - r, 2*r, 2*r)
		
		def drawString(s: String, cx: Double, cy: Double): Unit = {
			val fontMetrics = g2d.getFontMetrics
			val layout = new TextLayout(s, g2d.getFont, g2d.getFontRenderContext)
			
			val x = cx - layout.getBounds().getWidth / 2.0
			val y = cy + layout.getBounds().getHeight / 2.0
			layout.draw(g2d, x.toFloat, y.toFloat)
		}
		
		g2d.translate(cx - menuDimensions.width / 2, cy - menuDimensions.height / 2)

		if (!working) {
			menuAreas.foreach(area => {
			  if (area.menuItem.text != "Cancel" && menuGraph.outDegreeOf(area.menuItem) > 0)
			    g2d.setColor(new Color(54, 54, 153, 180).brighter)
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
//				drawString(area.menuItem.text, area.center.x, area.center.y)
				val t = g2d.getTransform()
	      g2d.rotate(if (math.cos(area.cAngle) < 0) area.cAngle + math.Pi else area.cAngle, area.center.x, area.center.y)
	      drawString(area.menuItem.text, area.center.x, area.center.y)
	      g2d.setTransform(t)
			})
		
		} else {
		  g2d.setColor(new Color(0, 0, 0, 200))
		  g2d.fillOval(-10, -10, menuDimensions.width + 20, menuDimensions.height + 20)
		}
		
		// Draw progress bar
		g2d.setColor(new Color(0, 0, 180, 180))
		progressAreas.foreach(area => {
		  if (area.progress < taskProgress) {
			  g2d.fill(area)
			  g2d.draw(area)
		  }
		})
		
		if (taskProgress < 0) {
			//g2d.setColor(new Color(54, 54, 153, (180 * alpha).toInt))
		  val oldComposite = g2d.getComposite
		  g2d.setComposite(AlphaComposite.SrcOver.derive(alpha))
		  g2d.drawImage(indeterminateProgressImage, -50, -50, null)
		  g2d.setComposite(oldComposite)
			//g2d.fill(indeterminateArea)
		}
		
		// Draw status text
		if (taskText != "") {
//		  g2d.setColor(new Color(40, 40, 40, 180))
//		  g2d.fillRect(0, menuDimensions.height + 55, menuDimensions.width, 30)
			g2d.setColor(new Color(220, 220, 255))
			drawString(taskText, menuDimensions.width / 2, menuDimensions.height / 2)
		}
			
//		g2d.setColor(new Color(54, 54, 153, 180))
//		g2d.fill(okRegion)
//		g2d.setColor(new Color(10, 10, 153, 255))
//		g2d.draw(okRegion)
//		g2d.setColor(new Color(220, 220, 255))
//		drawString("ok", okRegion.getBounds.getCenterX, okRegion.getBounds.getCenterY)
//		
//		g2d.setColor(new Color(151, 38, 38, 180))
//		g2d.fill(cancelRegion)
//		g2d.setColor(new Color(151, 10, 10, 180))
//		g2d.draw(cancelRegion)
//		g2d.setColor(new Color(255, 220, 220))
//		drawString("cancel", cancelRegion.getBounds.getCenterX, cancelRegion.getBounds.getCenterY)
//		
//		g2d.setColor(Color.orange)
//		g2d.draw(wedge)
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
				menuDimensions.width + 2*pad, menuDimensions.height + 2*pad)
		repaint(dirtyArea)
	}
	
	reactions += {
	  case RingMenuUpdate(newMenuGraph) => {
	    menuGraph = newMenuGraph
	  }
		case e: MouseClicked => {
		  if (!working) {
			mouseToMenu(e.point) match {
				case Some((x, y)) => {
					menuAreas.find(_.contains(x, y)) match {
						case Some(area) => {
							if (area.menuItem.text == "Cancel") {
								// Try to go back up the menu
								if (curRoot.text == "Root") {
									visible = false
								} else {
									val parent = menuGraph.getEdgeSource(menuGraph.incomingEdgesOf(curRoot).asScala.head)
									curRoot = parent
									initAreas()
									repaintDirtyMenu()
								}
							} else if (menuGraph.outDegreeOf(area.menuItem) == 0) {
								// Reached a menu leaf
							  
							  val progListener = new RingMenuProgressListener {
							    override def postProgress(text: String, progress: Float): Unit = {
							      javax.swing.SwingUtilities.invokeAndWait(new Runnable {
							        override def run = {
							          taskText = text
									      taskProgress = -1
									      repaint()
							        }
							      })
							    }
							  }
							  scala.actors.Actor.actor {
							    working = true
							    indeterminateAlphaTimer.start
								  area.menuItem.handleClick(progListener)
								  taskText = ""
								  taskProgress = 0
								  indeterminateAlphaTimer.stop
								  visible = false
								  working = false
							  }
							} else {
								// Pull up the sub-menu
								curRoot = area.menuItem
//								area.menuItem.handleClick()
								initAreas()
								repaintDirtyMenu()
							}
						}
						case None => {visible = false}
					}
				}
				case None => {visible = false}
			}
		  }
		}
		case e: MouseEntered => {
		  if (!working) {
			menuAreas.foreach(_.highlighted = false)
			repaintDirtyMenu()
		  }
		}
		case e: MouseExited => {
		  if (!working) {
			menuAreas.foreach(_.highlighted = false)
			repaintDirtyMenu()
		  }
		}
		case e: MouseMoved => {
		  if (!working) {
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
				case None => {menuAreas.foreach(_.highlighted = false)}
			}
		}
		}
	}
	
	override def visible_=(visible: Boolean): Unit = {
		if (visible) {
			curRoot = getDefaultRoot
			initAreas
			backdrop = GraphicsUtilities.createCompatibleImage(mainFrame.peer.getRootPane().getWidth(),
                                                               mainFrame.peer.getRootPane().getHeight());
			val g2 = backdrop.createGraphics();
			mainFrame.peer.getRootPane().paint(g2);
      g2.dispose();
      
      backdrop = GraphicsUtilities.createThumbnail(backdrop, mainFrame.peer.getRootPane().getWidth() / 2);
      backdrop = new ColorTintFilter(Color.BLACK, 0.10f).filter(backdrop, null);
      backdrop = new GaussianBlurFilter(12).filter(backdrop, null)
		} else {
			if (backdrop != null)
				backdrop.flush()
			backdrop = null
		}
		super.visible_=(visible)
	}
	
	var menuAreas: List[MenuArea] = null
	var progressAreas: List[ProgressArea] = null
//	var indeterminateArea: ProgressArea = null
	var indeterminateProgressImage: BufferedImage = null
	
	def initAreas(): Unit = {
		val brect = new Rectangle(0, 0, menuDimensions.width, menuDimensions.height)
		val cx = brect.x + brect.width / 2
		val cy = brect.y + brect.height / 2
		
		val cancelRad = 30
		val cancelArea = new MenuArea(new Ellipse2D.Double(cx - cancelRad, cy - cancelRad, 2*cancelRad, 2*cancelRad), new RingMenuItem("Cancel"), new Point2D.Double(cx, cy), 0)
		
		val numChildren = menuGraph.outgoingEdgesOf(curRoot).size
		val angleGap = math.toRadians(10)	// radians
		val centerGap = 10	// pixels
		val rectInner = new Rectangle2D.Double(cx - cancelRad - centerGap, cy - cancelRad - centerGap, 2*(cancelRad + centerGap), 2*(cancelRad + centerGap))
		val rectOuter = new Rectangle2D.Double(0, 0, menuDimensions.width, menuDimensions.height)
		
		// Each child will subtend this angle
		val subtendAngle = (2.0 * math.Pi) / numChildren.toDouble
    val childAreas = menuGraph.outgoingEdgesOf(curRoot).asScala.map(e => menuGraph.getEdgeTarget(e)).toIndexedSeq.sortBy[String](_.text)(new WindowsExplorerStringOrdering).reverse.zipWithIndex.map {
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

        new MenuArea(new Area(gp), child, center, cAngle)
      }
    }
		
		initProgressBarAreas
		
		menuAreas = List(cancelArea) ++ childAreas
	}
	
	def initProgressBarAreas: Unit = {
	  val brect = new Rectangle(-50, -50, menuDimensions.width + 100, menuDimensions.height + 100)
	  val cx = brect.x + brect.width / 2
		val cy = brect.y + brect.height / 2
		
		val numAreas = 64
		val angleGap = math.toRadians(1)	// radians
		val centerGap = 10
		val rectInner = new Rectangle2D.Double(-centerGap, -centerGap, menuDimensions.width + 2*centerGap, menuDimensions.height + 2*centerGap)
	  val rectOuter = new Rectangle2D.Double(-50, -50, menuDimensions.width + 100, menuDimensions.height + 100)
	  
	  val subtendAngle = (2.0 * math.Pi) / numAreas.toDouble
	  val areas = (0 until numAreas).map(i => {
	    val theta1 = i * subtendAngle + angleGap / 2.0
	    val theta2 = theta1 + subtendAngle - angleGap
	    val theta1Deg = math.toDegrees(theta1)
	    val theta2Deg = math.toDegrees(theta2)
	    val gp = new GeneralPath
	    gp.append(new Arc2D.Double(rectInner.x, rectInner.y, rectInner.width, rectInner.height, theta1Deg, theta2Deg - theta1Deg, Arc2D.OPEN), true)
			gp.append(new Arc2D.Double(rectOuter.x, rectOuter.y, rectOuter.width, rectOuter.height, theta2Deg, theta1Deg - theta2Deg, Arc2D.OPEN), true)
			gp.closePath
			
			// Compute the center of this area (for displaying text)
			val cAngle = -(theta1 + theta2) / 2.0
			val pInner = new Point2D.Double(cx + menuDimensions.width*math.cos(cAngle), cy + menuDimensions.width*math.sin(cAngle))
			val outerRad = menuDimensions.width/2.0
			val pOuter = new Point2D.Double(cx + outerRad*math.cos(cAngle), cy + outerRad*math.sin(cAngle))
			val center = new Point2D.Double((pInner.x + pOuter.x) / 2.0, (pInner.y + pOuter.y) / 2.0)
	    new ProgressArea(new Area(gp), (i+1) / numAreas.toFloat)
	  })
	  progressAreas = areas.toList
	  
	  indeterminateProgressImage = {
	    val innerOval = new Area(new Ellipse2D.Double(rectInner.x, rectInner.y, rectInner.width, rectInner.height))
	    val outerOval = new Area(new Ellipse2D.Double(rectOuter.x, rectOuter.y, rectOuter.width, rectOuter.height))
	    outerOval.subtract(innerOval)
	    val img = GraphicsUtilities.createTranslucentCompatibleImage(rectOuter.width.toInt, rectOuter.height.toInt)
	    val g2d = img.createGraphics
	    g2d.translate(-rectOuter.x, -rectOuter.y)
	    g2d.setColor(new Color(54, 54, 153))
	    g2d.fill(outerOval)
	    
	    img
	  }
	  
	}
	
	
//	def initRegions(): Unit = {
//		val brect = new Rectangle(0, 0, menuDimensions.width, menuDimensions.height)
//		val cx = brect.x + brect.width / 2
//		val cy = brect.y + brect.height / 2
//		
//		val middleRad = 40d
//		
//		val gapDiam = 2
//		val thetaStart = math.atan2(gapDiam, math.sqrt(middleRad*middleRad - gapDiam*gapDiam))
//		val thetaEnd = math.Pi - thetaStart
//		val thetaRange = thetaEnd - thetaStart
//		val n = 16
//		val angles = (0 to n).map(i => thetaStart + (i/n.toDouble)*thetaRange)
//		val okPath = new Path2D.Double
//		okPath.moveTo(middleRad*math.cos(angles.head), -middleRad*math.sin(angles.head))
//		angles.foreach(theta => {okPath.lineTo(middleRad * math.cos(theta), -middleRad * math.sin(theta))})
//		okPath.closePath
//		okPath.transform(AffineTransform.getTranslateInstance(cx, cy))
//		okRegion = new Area(okPath)
//		
//		val cancelTransform = new AffineTransform
//		cancelTransform.translate(cx, cy)
//		cancelTransform.scale(1, -1)
//		cancelTransform.translate(-cx, -cy)
//		cancelRegion = new Area(okPath)
//		cancelRegion.transform(cancelTransform)
//		
//		val innerArcRect = new Rectangle2D.Double(cx - middleRad - 10, cy - middleRad - 10, 2*middleRad+20, 2*middleRad+20)
//		val innerArc = new Arc2D.Double(innerArcRect.x, innerArcRect.y, innerArcRect.width, innerArcRect.height, 0, math.Pi / 4.0, Arc2D.OPEN)
//		
//		wedge = new Area(innerArc)
//		
////		val middleCircle = new  Ellipse2D.Double(cx - middleRadius, cy - middleRadius, 2*middleRadius, 2*middleRadius)
////		val sepAmount = 5
////		val sep = new Rectangle2D.Double(cx - middleRadius, cy - sepAmount, middleCircle.width, cy + sepAmount)
////		
//		
//	}
}
