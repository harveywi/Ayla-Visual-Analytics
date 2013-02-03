/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui

import javax.media.j3d.Behavior
import javax.media.j3d.WakeupOnElapsedFrames

class MutatorBehavior extends Behavior {
  
  private var mutationFunction: Option[() => Unit] = None
  
  val wakeCriterion = new WakeupOnElapsedFrames(0, false)
  override def initialize(): Unit = {
     wakeupOn(wakeCriterion)
  }
  
  def apply(f: () => Unit): Unit = {
    mutationFunction = Some(f)
  }

  override def processStimulus(criteria: java.util.Enumeration[_]): Unit = {
    mutationFunction match {
      case Some(function) => {
        println("Mutating");
        function();
        mutationFunction = None
        println("Done")
      }
      case None => {}
    }
    
    wakeupOn(wakeCriterion)
  }
}
