/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui

import javax.media.j3d.BranchGroup
import javax.media.j3d.BoundingSphere
import javax.vecmath.Point3d

class MutatorPointCloudView extends PointCloudView {
  val mutator = new MutatorBehavior()
	mutator.setSchedulingBounds(new BoundingSphere(new Point3d(), 1000.0))
	val mutatorBG = new BranchGroup()
	mutatorBG.addChild(mutator)
	objRoot.addChild(mutatorBG)
}
