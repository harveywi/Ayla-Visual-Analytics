package edu.osu.compgeom.omegavis.event

import javax.media.j3d.{PointArray, IndexedTriangleArray}
import javax.vecmath.Point3d
import javax.media.j3d.Shape3D
import javax.media.j3d.LineArray
import javax.vecmath.Color4f
import scala.swing.event.Event
import edu.osu.compgeom.topology.ScalarFunction
import edu.osu.compgeom.omegavis._
import edu.osu.compgeom.ct._
import edu.osu.compgeom.landscapes.floorplan.VoronoiFloorplan
import java.io.File
import org.jgrapht.graph.SimpleDirectedGraph
import org.jgrapht.graph.DefaultEdge
import javax.media.j3d.LineArray
import edu.osu.compgeom.dataset.CachedDataset
import javax.media.j3d._

case class DatasetLoadRequest(val datasetDir: java.io.File) extends Event
case class DatasetLoaded(dataset: CachedDataset) extends Event
case class GetKnnGraphParamsRequest() extends Event
case class UpdateKnnGraphRequest(k: Int) extends Event
case class ScalarFunctionReady(sf: ScalarFunction) extends Event
case class ContourTreeReady(ct: ContourTree) extends Event
case class SimplifyContourTree(tau: Float) extends Event
case class ColormapUpdate(colormap:  GammaColormap, sf: ScalarFunction) extends Event
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
