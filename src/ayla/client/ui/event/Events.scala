/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui.event

import javax.media.j3d.{ PointArray, IndexedTriangleArray }
import javax.vecmath.Point3d
import javax.media.j3d.Shape3D
import javax.media.j3d.LineArray
import javax.vecmath.Color4f
import scala.swing.event.Event
import ayla.geometry.ScalarFunction
import ayla.geometry.ct._
import java.io.File
import org.jgrapht.graph.SimpleDirectedGraph
import org.jgrapht.graph.DefaultEdge
import javax.media.j3d.LineArray
import ayla.dataset.CachedDataset
import javax.media.j3d._
import ayla.landscape.VoronoiFloorplan
import ayla.colormap.AylaColormap
import ayla.client.ui.TopologicalComponentManager
import ayla.client.ui.menu.RingMenuItem
import ayla.collab._

case class DatasetLoadRequest(val datasetDir: java.io.File) extends Event
case class DatasetLoaded(dataset: CachedDataset) extends Event
case class GetKnnGraphParamsRequest() extends Event
case class UpdateKnnGraphRequest(k: Int) extends Event
case class ScalarFunctionReady(sf: ScalarFunction) extends Event
case class ContourTreeReady(ct: ContourTree) extends Event
case class SimplifyContourTree(tau: Float) extends Event
case class ColormapUpdate(colormap: AylaColormap, sf: ScalarFunction) extends Event
case class SelectionUpdated(selectedEdges: Set[ContourTreeEdge]) extends Event
case class SelectionFinalized() extends Event
case class TerrainUpdatedEvent(floorplan: VoronoiFloorplan, ct: ContourTree) extends Event
case class ContourTreeViewUpdatedEvent(lines: Array[Shape3D], ct: ContourTree) extends Event
case class EdgeSelected(ctEdge: ContourTreeEdge, selectionID: Int) extends Event
case class NodeSelected(ctNode: ContourTreeNode) extends Event
case class SetMorseFunction(morseFunctionFile: File) extends Event

case class SelectionCleared() extends Event
case class SetCameraEvent(rotationCenter: Point3d, zoom: Double) extends Event
case class PointCloudViewUpdated(pointArray: PointArray, ct: ContourTree) extends Event
case class TriangulatedPointCloudViewUpdated(indexedTriArray: IndexedTriangleArray) extends Event

case class TopologicalComponentManagerSelectionUpdate(tcManager: TopologicalComponentManager) extends Event
case class MutateGeometry(mutatorFunction: () => Unit) extends Event

case class RingMenuUpdate(newMenuGraph: SimpleDirectedGraph[RingMenuItem, DefaultEdge]) extends Event
case class PathReady(lineArray: LineArray) extends Event
case class TerrainSpheresReady(val sphereBranchGroup: BranchGroup) extends Event

case class SetS1(conformationID: Int) extends Event

case class TerrainPanelClicked(p: java.awt.Point) extends Event

case class ConformationPointClicked(conformationID: Int, pdbLines: Array[String]) extends Event

case class AnnotationsRefreshed(annotations: Array[ConformationAnnotation]) extends Event
case class StoryboardsRefreshed(annotations: Array[Storyboard]) extends Event

case object RefreshAnnotationVisibilities extends Event
