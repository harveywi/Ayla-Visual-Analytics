package edu.osu.compgeom.omegavis

import edu.osu.compgeom.omegavis.behavior.MutatorBehavior
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