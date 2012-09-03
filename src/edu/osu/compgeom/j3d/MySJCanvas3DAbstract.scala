package edu.osu.compgeom.j3d

import java.awt.{AWTEvent, Container, Graphics2D, GraphicsConfigTemplate,
  GraphicsConfiguration, GraphicsDevice, GraphicsEnvironment, Rectangle}
import java.awt.event.{ComponentEvent, FocusEvent, KeyEvent, MouseEvent, MouseWheelEvent}
import java.awt.image.BufferedImage
import java.util.concurrent.locks.{Condition, ReentrantLock}

// Java 3D
import javax.media.j3d.{Canvas3D, GraphicsConfigTemplate3D, ImageComponent, 
  ImageComponent2D, RestrictedAccessException, Screen3D}
import com.sun.j3d.exp.swing.impl.AutoOffScreenCanvas3D

// Java Swing
import javax.swing.{JPanel, SwingUtilities}
import javax.swing.event.{AncestorEvent, AncestorListener}

// Scala Swing
import scala.swing.{Component, Swing}

/** The class MySJCanvas3DAbstract provides an abstract lightweight 
 *  Scala Swing component that Java 3D can render into.
 *  
 *  @author August Lammersdorf, InteractiveMesh
 *  @version 1.3 - 2011/09/19
 */
abstract class MySJCanvas3DAbstract(
	private val device: GraphicsDevice, 
    private val template: GraphicsConfigTemplate3D, additionalPostRenderMethod: Canvas3D => Unit) 
      extends Component { outer: Component =>  					

  // throws java.lang.IllegalArgumentException
  require(device ne null, "device is null !")
  require(template ne null, "template is null !")
  
  //
  // Component
  //
	  
  // Onscreen JPanel which Java 3D is 'rendering' into
  override lazy val peer: JPanel = new JPanel with CanvasSuperMixin  
  
  /** This trait is used to redirect certain calls from the peer to the wrapper 
   *  and back. Useful to expose methods that can be customized by overriding.
   */
  protected trait CanvasSuperMixin extends SuperMixin {
	  
	def doubleBuffered: Boolean = peer.isDoubleBuffered
	def doubleBuffered_=(b: Boolean) = peer.setDoubleBuffered(b)
	  
    //
    // Adjust offscreen buffer when onscreen JPanel is resized
    //
	  
    override def setBounds(x: Int, y: Int, width: Int, height: Int) =
      outer.bounds = (x, y, width, height)
    def __super__setBounds(x: Int, y: Int, width: Int, height: Int) {
      super.setBounds(x, y, width, height)
    }

    //
    // Redirect events from onscreen JPanel to offscreen Canvas3D
    //

    def enableAWTEvents(eventsToEnable: Long) {
	  super.enableEvents(eventsToEnable)
    }
    def disableAWTEvents(eventsToEnable: Long) {
	  super.disableEvents(eventsToEnable)
    }

    override protected def processComponentEvent(e: ComponentEvent) { 
      super.processComponentEvent(e)
//      if ((wakeupEventMasks & AWTEvent.COMPONENT_EVENT_MASK) != 0) {
        val src = e.getSource
        e.setSource(canvas)
        canvas.processComponentEvent(e)
        e.setSource(src)
//      }
    }
    override protected def processFocusEvent(e: FocusEvent) {
      super.processFocusEvent(e)
      if ((wakeupEventMasks & AWTEvent.FOCUS_EVENT_MASK) != 0) {
        val src = e.getSource
        e.setSource(canvas)
        canvas.processFocusEvent(e)
        e.setSource(src)
      }
    }
    override def processKeyEvent(e: KeyEvent) {
      super.processKeyEvent(e)
      if ((wakeupEventMasks & AWTEvent.KEY_EVENT_MASK) != 0) {
        val src = e.getSource
        e.setSource(canvas)
        canvas.processKeyEvent(e)
        e.setSource(src)
      }
    }
    override def processMouseEvent(e: MouseEvent) {
      super.processMouseEvent(e)
      if ((wakeupEventMasks & AWTEvent.MOUSE_EVENT_MASK) != 0) {
        val src = e.getSource
        e.setSource(canvas)
        canvas.processMouseEvent(e)
        e.setSource(src)
      }
    }
    override def processMouseMotionEvent(e: MouseEvent) {
      super.processMouseMotionEvent(e)
      if ((wakeupEventMasks & AWTEvent.MOUSE_MOTION_EVENT_MASK) != 0) {
        val src = e.getSource
        e.setSource(canvas)
        canvas.processMouseMotionEvent(e)
        e.setSource(src)
      }
    }
    override def processMouseWheelEvent(e: MouseWheelEvent) {
      super.processMouseWheelEvent(e)
      if ((wakeupEventMasks & AWTEvent.MOUSE_WHEEL_EVENT_MASK) != 0) {
        val src = e.getSource
        e.setSource(canvas)
        canvas.processMouseWheelEvent(e)
        e.setSource(src)
      }
    }
  }
  
  // (x, y, with, height)
  def bounds_=(bnd: (Int, Int, Int, Int)) {
    peer match {
      case peer: CanvasSuperMixin => 
        peer.__super__setBounds(bnd._1, bnd._2, bnd._3, bnd._4)
        createOffScreenBuffer(bnd._3, bnd._4)
      case _ => 
    }
  }

  // key events can be received
  focusable = true 
  // no double buffer needed
  peer.asInstanceOf[CanvasSuperMixin].doubleBuffered = false
 
  //
  // AncestorListener (for internal use only)
  //
  
  import scala.swing.UIElement
  import scala.swing.event.UIEvent
  
  private case class AncestorAdded(source: UIElement) extends UIEvent
  private case class AncestorMoved(source: UIElement) extends UIEvent
  private case class AncestorRemoved(source: UIElement) extends UIEvent

  override def onFirstSubscribe {
	  
    super.onFirstSubscribe
    
    peer.addAncestorListener(new AncestorListener {
      def ancestorAdded(e: AncestorEvent) { 
        publish(AncestorAdded(outer)) 
      }
      def ancestorMoved(e: AncestorEvent) { 
        publish(AncestorMoved(outer)) 
      }
      def ancestorRemoved(e: AncestorEvent) { 
        publish(AncestorRemoved(outer))         
      }
    })
  }
  
  listenTo(outer)
  reactions += {
    case e: AncestorAdded =>
        
      var szWidth = outer.size.width
      var szHeight = outer.size.height

      if (szWidth == 0) {
        szWidth = 100
      }

      if (szHeight == 0) {
        szHeight = 100
      }
      
      createOffScreenBuffer(szWidth, szHeight)

      canvas.addNotifyFlag = true
      canvas.addNotify
      
    case e: AncestorMoved => {}
      
    case e: AncestorRemoved => canvas.removeNotify
  }
  
  //
  // Sync writing and drawing of the '3D' image / offscreen buffer
  //
  
  protected val imageAccessLock: ReentrantLock = new ReentrantLock
  protected val imagePaintCondition: Condition = imageAccessLock.newCondition
  @volatile protected var isImageDrawn: Boolean = false
  
  //
  // Offscreen Canvas3D
  //
  
  // Force double-buffer and stereo to UNNECESSARY
  template.setStereo(GraphicsConfigTemplate.UNNECESSARY)
  template.setDoubleBuffer(GraphicsConfigTemplate.UNNECESSARY)
  
  // the graphics configuration used for this canvas 
  private var graphicsConfig = device.getBestConfiguration(template)
  
  //
  // Offscreen buffer
  //
  
  // That image of the off-screen buffers which Swing is drawing.
  protected var paintImage: BufferedImage = null
  // The width of the image which Swing is drawing.
  @volatile protected var imageWidth: Int = 0     
  // The height of the image which Swing is drawing.
  @volatile protected var imageHeight: Int = 0     
    
/*
protected var startTimePre: Long = 0
protected var startTimeSwap: Long = 0
protected var endTimeSwap: Long = 0
protected var startTimeRender: Long = 0
protected var endTimeRender: Long = 0

protected var waitTimeSwap: Long = 0
protected var waitTimeEDT: Long = 0
*/  
  
  // Java 3D WakeupOnAWTEvents: 
  //   COMPONENT_EVENT_MASK, FOCUS_EVENT_MASK, 
  //   KEY_EVENT_MASK
  //   MOUSE_EVENT_MASK, MOUSE_MOTION_EVENT_MASK, MOUSE_WHEEL_EVENT_MASK
  
  //
  private var wakeupEventMasks: Long = 0L

  /**
   * Sets the AWT event masks for which this MySJCanvas3DAbstract instance 
   * will call the corresponding process-methods on the underlying heavyweight 
   * Canvas3D isntance.
   * 
   * This is required only when Java 3D WakeupOnAWTEvents are specified in subclasses of Behavior. 
   * 
   * No event mask is set per default.
   * 
   * @param eventMasks ored AWT event masks: <code>AWTEvent.COMPONENT_EVENT_MASK,
   * AWTEvent.FOCUS_EVENT_MASK, AWTEvent.KEY_EVENT_MASK, AWTEvent.MOUSE_EVENT_MASK,
   * AWTEvent.MOUSE_MOTION_EVENT_MASK, AWTEvent.MOUSE_WHEEL_EVENT_MASK</code>.
   */
  def enableWakeupOnAWTEvents(eventMasks: Long) {      
    
    if ((eventMasks & AWTEvent.COMPONENT_EVENT_MASK) != 0)
        wakeupEventMasks |= AWTEvent.COMPONENT_EVENT_MASK
    if ((eventMasks & AWTEvent.FOCUS_EVENT_MASK) != 0)
        wakeupEventMasks |= AWTEvent.FOCUS_EVENT_MASK
    if ((eventMasks & AWTEvent.KEY_EVENT_MASK) != 0)
        wakeupEventMasks |= AWTEvent.KEY_EVENT_MASK
    if ((eventMasks & AWTEvent.MOUSE_EVENT_MASK) != 0)
        wakeupEventMasks |= AWTEvent.MOUSE_EVENT_MASK
    if ((eventMasks & AWTEvent.MOUSE_MOTION_EVENT_MASK) != 0)
        wakeupEventMasks |= AWTEvent.MOUSE_MOTION_EVENT_MASK
    if ((eventMasks & AWTEvent.MOUSE_WHEEL_EVENT_MASK) != 0)
        wakeupEventMasks |= AWTEvent.MOUSE_WHEEL_EVENT_MASK     
    /*
    Enables the events defined by the specified event mask parameter 
    to be delivered to this component.

    Event types are automatically enabled when a listener for that event type 
    is added to the component.

    This method only needs to be invoked by subclasses of Component 
    which desire to have the specified event types delivered 
    to processEvent regardless of whether or not a listener is registered. 
    */
    peer.asInstanceOf[CanvasSuperMixin].enableAWTEvents(wakeupEventMasks)
  }
  /**
   * Disables calling event process-methods on the underlying heavyweight Canvas3D
   * instance for the specified AWT event masks.
   * @param eventMasks ored AWT event masks: <code>AWTEvent.COMPONENT_EVENT_MASK,
   * AWTEvent.FOCUS_EVENT_MASK, AWTEvent.KEY_EVENT_MASK, AWTEvent.MOUSE_EVENT_MASK,
   * AWTEvent.MOUSE_MOTION_EVENT_MASK, AWTEvent.MOUSE_WHEEL_EVENT_MASK</code>
   */
  def disableWakeupOnAWTEvents(eventMasks: Long) {
    wakeupEventMasks &= ~eventMasks
    peer.asInstanceOf[CanvasSuperMixin].disableAWTEvents(wakeupEventMasks)
  }

  /**
   * Returns the wrapped component which Java 3D renders into.
   * @return the lightweight Java Swing component
   */
  def onscreenJPanel: JPanel = {
	return peer
  }
  /**
   * Returns the offscreen Canvas3D instance of this lightweight component.
   * 
   * @return the offscreen Canvas3D instance that is linked to this onscreen component
   */
  def offscreenCanvas3D: Canvas3D = {
    return canvas
  }

  /**
   * Creates a new offscreen buffer of the given size. This method is called 
   * internally whenever this panel is added to a parent or is resized. 
   * <p>
   * Subclasses should call and/or override this method according to its
   * individual needs. In case of overriding calling
   * <code>super.createOffScreenBuffer(bufferWidth, bufferHeight)</code> 
   * has to be the last thing to do.</p>
   * @param width the width of the off-screen buffers to create
   * @param height the height of the off-screen buffers to create
   */
  protected def createOffScreenBuffer(width: Int, height: Int) {        
    setupCanvas(width, height)  
  }

  /**
   * Callback used to allow an overriding subclass to execute individual code
   * when a new offscreen buffer was created. 
   * <p>
   * This method is called internally by the event-dispatching thread (EDT)
   * and should not be called by applications. 
   * </p>
   */
  protected def offScreenBufferCreated() {
	  
  }

  /**
   * Callback used to allow an overriding subclass to execute individual code
   * when the offscreen buffer was copied.
   * <p>
   * This method is called internally by the event-dispatching thread (EDT)
   * and should not be called by applications. 
   * </p>
   */
  protected def offScreenBufferCopied() { 
	  
  }

  /**
   * Subclasses which overrides <code>paintComponent(Graphics g)</code>
   * should check if the offscreen buffer is ready for drawing.
   * 
   * @return true if offscreen buffer can be drawn
   */
  protected final def isReadyForDrawing: Boolean = {
    ((paintImage ne null) && canvas.isRendererRunning)
  }    

  //
  private def setupCanvas(width: Int, height: Int) {
    
    val w: Int = if (width > 0) width else 1
    val h: Int = if (height > 0) height else 1

    if (canvas.getWidth == w && canvas.getHeight == h) 
      return

    // Size changed: create a new offScreen buffer on EDT

    if (SwingUtilities.isEventDispatchThread) {
      canvas.createOffScreenBuffer(graphicsConfig.getBounds, w, h)
    }
    else {
      Swing.onEDT(canvas.createOffScreenBuffer(graphicsConfig.getBounds, w, h))
    }
  }
  
  /**
   * Flips and paints the result of the 3D rendering.
   */
  override protected def paintComponent(g2d: Graphics2D) {
    
    peer match {
      case peer: SuperMixin => super.paintComponent(g2d)
         
	    // To be implemented in subclasses.	    
/*	    
	    // Draw & flip offscreen buffer        
	    if (isReadyForDrawing) {          
	                  
	      var isLocked: Boolean = false
	
	      try {
	        // Avoids deadlock when J3D-Renderer thread acquires the lock 
	        // in 'postRender' but don't call 'postSwap' due to J3D internal states
	        isLocked = imageAccessLock.tryLock(50, TimeUnit.MILLISECONDS)
	            
	        g2D.drawImage(paintImage,
	                    // destination in g2d: flip lowerY and upperY 
	                    // dx1    dy1          dx2         dy2
	                        0, imageHeight, imageWidth, 0,    
	                    // source: the bottom part of the scene image
	                    // sx1    sy1          sx2         sy2
	                        0, 0,           imageWidth, imageHeight, null)
	
	         // Release J3D-Renderer thread
	         isImageDrawn = true
	         if (isLocked)
	           imagePaintCondition.signal              
	       }         
	       finally {  
	    	 if (isLocked)
	           imageAccessLock.unlock
	       }
	    }
*/
            
      case _ => {}
    }
  }
  
  //
  // Auto-offscreen Canvas3D (Scala class derived from Java class & interface)
  //
  
  // The offscreen Canvas3D that is linked to this onscreen Component
  private object canvas extends Canvas3D(graphicsConfig, true) with AutoOffScreenCanvas3D {
	  
    private val imageResizeCondition: Condition = imageAccessLock.newCondition
    
    @volatile private var isRendererLocked: Boolean = false  
    @volatile private var isWatingForResizing: Boolean = false  
    
    private var isSwapStarted: Boolean = false 

    /**
     * Flag used to sort a call to addnotify() from user and
     * from the lightweight component. Lightweight component calls
     * addNotify() so that the rendering begins and uses normal routines,
     * but this is a method that user must not call.
     */
    private[MySJCanvas3DAbstract] var addNotifyFlag: Boolean = false
    
    // Don't wait longer than 200 ms for repainting
    private val LIMIT: Int = 200000000

	private val METERS_PER_PIXEL: Double = 0.0254 / 90.0
	
	// Repainter
    /** 
     * Invokes callback method 'offScreenBufferCopied' and
     * repaints the lightweight panel while the J3D-Renderer thread is waiting.
     */
    private final val repainter = Swing.Runnable({               
      offScreenBufferCopied
      outer.repaint
    })

    // To be called on the EDT only !!!!!!!!!!!!!!
    /**
     * Creates an offscreen buffer to be attached to the heavyweight
     * buffer. Buffer is created 'byreference'
     *
     * @param width the width of the buffer.
     * @param height the height of the buffer.
     */
    private[MySJCanvas3DAbstract] def createOffScreenBuffer(screenRect: Rectangle, width: Int, height: Int) {

      try {
        imageAccessLock.lock
            
        // 
        this.stopRenderer
            
        isWatingForResizing = true

        // Setting offscreen buffer requires that the J3D-Renderer thread isn't blocked
        // As we are on the EDT, 'this.run()' will not release the J3D-Renderer
        // So, it's done here. We will wait until this has happened.
        while (isRendererLocked) {
                
          isImageDrawn = true
          imagePaintCondition.signal
                 
          try {
            imageResizeCondition.await
          }
          catch {
            case e: InterruptedException => {}
            case other => {}
          }
        }
            
        isWatingForResizing = false
            
        // Offscreen rendering might occur even if the renderer is stopped. 
        // For that reason, we are waiting for a hypothetical offscreen render 
        // to finish before setting offscreen rendering.
        // Otherwise, rendering will stop with an exception.
        this.waitForOffScreenRendering
            
            
        // Compute physical dimensions of the screen
        // Fix to Issue : 433 - JCanvas3D crashed when using jogl pipe.
        val screenWidth: Int = screenRect.getWidth.asInstanceOf[Int]
        val screenHeight: Int = screenRect.getHeight.asInstanceOf[Int]
        val screen3D = this.getScreen3D
        screen3D.setSize(screenWidth, screenHeight)
        screen3D.setPhysicalScreenWidth(screenWidth * METERS_PER_PIXEL)
        screen3D.setPhysicalScreenHeight(screenHeight * METERS_PER_PIXEL)
            
        if (paintImage != null)
          paintImage.flush
            
        // OffScreenBuffer: byReference & yUp
        paintImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
        val image = new ImageComponent2D(ImageComponent.FORMAT_RGBA, paintImage, true, true)
        image.setCapability(ImageComponent.ALLOW_IMAGE_WRITE)
            
        imageWidth  = width
        imageHeight = height
            
        // Callback: new off-screen buffer created
        offScreenBufferCreated
            
        try {
          // EDT will wait (sleep) until exchange of offscreen buffer is done 
          this.setOffScreenBuffer(image)                
          // This method includes 'this.setSize(offScreenCanvasSize)'
        }
        catch {
          case e: RestrictedAccessException =>
            // Try again, TODO riks of endless loop ?
            createOffScreenBuffer(screenRect, width, height)
          case other => println("Repeat : createOffScreenBuffer unknown exception = " + other)
        }
      }
      finally {
        imageAccessLock.unlock
      }

      this.startRenderer
    }

    override def addNotify: Unit = {
	  if (addNotifyFlag) {
        addNotifyFlag = false
        super.addNotify             
      } 
	}
	
    /**
     * Java 3D needs a parent for some operations, so we in
     * fact cheat it by returning the lightweight component.
     *
     */
    override def getParent: Container = {
      return peer
      //if (peer.getParent eq null) return peer // TODO
      //else return peer.getParent
    }
    
    override def preRender {
//startTimePre = System.nanoTime
      if (isSwapStarted) {
        isSwapStarted = false
        try {
          imageAccessLock.unlock                  
        }
        catch {    
          case e: IllegalMonitorStateException => {}
        }
      }
    }
    
    override def postRender {
//startTimeSwap = System.nanoTime
      if (this.isRendererRunning) {             
        imageAccessLock.lock           
        isSwapStarted = true 
      }
      additionalPostRenderMethod(this)
      val g2d = getGraphics2D()
      g2d.flush(true)
    }

    override def postSwap {
        
//endTimeSwap = System.nanoTime
                        
      if (isSwapStarted) {   
        
        isImageDrawn = false
        
        SwingUtilities.invokeLater(repainter)
        
        while (!isImageDrawn) {
            
          isRendererLocked = true
            
          try {                           
            imagePaintCondition.awaitNanos(LIMIT)   // Don't wait for ever
            isImageDrawn = true                     // and release yourself
          }
          catch {
            case e: InterruptedException => {}
          }

          isRendererLocked = false
          if (isWatingForResizing) {
            imageResizeCondition.signal
          }
        }
                
//waitTimeSwap = System.nanoTime
            
        isSwapStarted = false
        
        try {
          imageAccessLock.unlock                    
        }
        catch {     
          case e: IllegalMonitorStateException =>
            System.out.println("JCanvas3DSBAbstract postSwap IllegalMonitorStateException")              
        }            
/* 
endTimeRender = System.nanoTime
System.out.println("JCanvas3DSBAbstract render 3 = "
        + (startTimePre-startTimeRender) + " / " 
        + (endTimeSwap-startTimeSwap) + " / " 
        + (waitTimeSwap-endTimeSwap) + " / " 
        + (endTimeRender-startTimeRender))

startTimeRender = endTimeRender */ 
      } 
    }
    
    
    // ----------------- Java 3D engine ------------------------------------
    
    // WakeupOnAWTEvent: COMPONENT_EVENT_MASK, FOCUS_EVENT_MASK, 
    //                   KEY_EVENT_MASK
    //                   MOUSE_EVENT_MASK, MOUSE_MOTION_EVENT_MASK, MOUSE_WHEEL_EVENT_MASK

    // Canvas3D / WakeupOnAWTEvent
    // EventCatcher: FocusListener, KeyListener, 
    //               MouseListener, MouseMotionListener, MouseWheelListener
    
    // Canvas3D, all ancestors, Window
    // EventCatcher: ComponentListener
    
    // Canvas3D, all ancestors, Window
    // CanvasViewEventCatcher: ComponentAdapter (componentResized/componentMoved) 

    // Window
    // EventCatcher: WindowListener
    
    // Hint: Offscreen-Canvas3D: componentMoved, componentHidden, componentShown
    //                           are not relevant
    //                           componentResized is called due to 
    //                                            setSize-call in setOffScreenBuffer
    
    // ---------------------------------------------------------------------
    
    //
    // Component's processXXXEvent methods
    // Overriden so that the CanvasSuperMixin can access it.
    //

    override protected[MySJCanvas3DAbstract] def processComponentEvent(e: ComponentEvent) {
      super.processComponentEvent(e)
    }

    override protected[MySJCanvas3DAbstract] def processFocusEvent(e: FocusEvent) {
      super.processFocusEvent(e)
    }

    override protected[MySJCanvas3DAbstract] def processKeyEvent(e: KeyEvent) {
      super.processKeyEvent(e)
    }

    override protected[MySJCanvas3DAbstract] def processMouseEvent(e: MouseEvent) {
      super.processMouseEvent(e)
    }

    override protected[MySJCanvas3DAbstract] def processMouseMotionEvent(e: MouseEvent) {
      super.processMouseMotionEvent(e)
    }
    
    override protected[MySJCanvas3DAbstract] def processMouseWheelEvent(e: MouseWheelEvent) {
      super.processMouseWheelEvent(e)
    }	    

  } // End of object canvas
}