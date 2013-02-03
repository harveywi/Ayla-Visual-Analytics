/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.j3d

// Java
import java.awt.{Graphics2D, GraphicsDevice, GraphicsEnvironment}
import java.util.concurrent.TimeUnit
import javax.media.j3d.GraphicsConfigTemplate3D
import javax.media.j3d.Canvas3D

/** The class MySJCanvas3D provides a lightweight Scala Swing component 
 *  that Java 3D can render into.
 *  
 *  @author August Lammersdorf, InteractiveMesh
 *  @version 1.3 - 2011/09/19
 */
class MySJCanvas3D(
    device: GraphicsDevice,
    template: GraphicsConfigTemplate3D, additionalPostRenderMethod: Canvas3D => Unit) 
      extends MySJCanvas3DAbstract(device, template, additionalPostRenderMethod) {
	
  def this(template: GraphicsConfigTemplate3D, additionalPostRenderMethod: Canvas3D => Unit) = this(  
    GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice,
    template,
    additionalPostRenderMethod
  )

  def this(device: GraphicsDevice, additionalPostRenderMethod: Canvas3D => Unit) = this(device, new GraphicsConfigTemplate3D, additionalPostRenderMethod)

  
  /** Creates a new off-screen buffer of the given size. This method is called 
   *  internally whenever this panel is added to a parent or is resized. 
   *  <p>
   *  Subclasses should call and/or override this method according to its
   *  individual needs. In case of overriding calling
   *  <code>super.createOffScreenBuffer(canvasWidth, canvasHeight)</code> 
   *  has to be the last thing to do.</p>
   *  @param width the width of the off-screen buffers to create
   *  @param height the height of the off-screen buffers to create
   */
  override protected def createOffScreenBuffer(width: Int, height: Int) {   
	
    super.createOffScreenBuffer(width, height)
  }

  /**
   * Callback used to allow an overriding subclass to execute individual code
   * when a new off-screen buffer was created. 
   * <p>
   * This method is called internally by the event-dispatching thread (EDT)
   * and should not be called by applications. 
   * </p>
   */
  override protected def offScreenBufferCreated() {
	  
  }

  /**
   * Callback used to allow an overriding subclass to execute individual code
   * when the off-screen buffer was copied.
   * <p>
   * This method is called internally by the event-dispatching thread (EDT)
   * and should not be called by applications. 
   * </p>
   */
  override protected def offScreenBufferCopied() {
	  
  }

  // Flip and draw 
  override protected def paintComponent(g2D: Graphics2D) {
    peer match {
      case peer: CanvasSuperMixin => super.paintComponent(g2D)
            
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
      
      case _ => {}
    }
  }

}
