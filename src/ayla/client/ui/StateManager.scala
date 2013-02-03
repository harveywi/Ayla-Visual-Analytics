/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.client.ui

import java.io._
import scala.swing.Publisher
import scala.swing.Reactor
import ayla.dataset.Dataset
import ayla.client.ui.menu.RingMenuItem
import ayla.client.ui.event._
import ayla.geometry.ScalarFunction
import scala.actors.Actor._
import org.jgrapht.graph.{ SimpleDirectedGraph, DefaultEdge }
import ayla.geometry.ct._
import scala.swing.Dialog
import javax.vecmath.Color4f
import scala.collection.JavaConversions._
import javax.media.j3d._
import javax.vecmath.Point3f
import javax.vecmath.Point3d
import java.awt.Color
import ayla.util.UnionFind
import ayla.util.IO._
import javax.swing.JColorChooser
import ayla.dataset._
import ayla.colormap._
import ayla.client.ui.menu.RingMenuProgressListener
import java.util.regex.PatternSyntaxException

class StateManager(val dataset: Dataset, val morseFunction: ScalarFunction, val ctSimp: ContourTree,
  val nodeAtInfinity: ContourTreeNode, conformationalSpacePanel: ConformationalSpacePanel) extends Reactor with Publisher {

  var colormapGenerator: ScalarFunction => AylaColormap = getJetColormap _
  var colorFunction = morseFunction

  class SetColorFunctionMenuItem(text: String, val file: File) extends RingMenuItem(text) {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Loading color function", -1)

      //      val lines = scala.io.Source.fromFile(file).getLines
      //      val colorFuncVals = lines.drop(1).map(_.toFloat).toArray
      val colorFuncVals = dataset.getScalarArray(file)
      progressListener.postProgress("Applying colormap", -1)

      if (colorFuncVals.distinct.size == 2) {
        // We have a binary function
        showBinaryFunction(morseFunction, colorFuncVals, ctSimp)
      } else {
        showSecondaryStructure(morseFunction, colorFuncVals, ctSimp)
      }
    }
  }

  val menuRoot = new RingMenuItem("Root")

  /*
  val menuSetInfinityLow = new RingMenuItem("Set Infinity (Low)") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Constructing terrain", -1)
      val rootEdge = tcManager.componentMap.find(p => p._2.selectionStatus != 0).get._1
      val pointAtInfinity = if (morseFunction.vc.compare(rootEdge.n1.vertex, rootEdge.n2.vertex) < 0) rootEdge.n1 else rootEdge.n2
      StateManager.this.publish(new RegenerateTerrainEvent(ctSimp, pointAtInfinity))
    }
  }
  val menuSetInfinityHigh = new RingMenuItem("Set Infinity (High)") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Constructing terrain", -1)
      val rootEdge = tcManager.componentMap.find(p => p._2.selectionStatus != 0).get._1
      val pointAtInfinity = if (morseFunction.vc.compare(rootEdge.n1.vertex, rootEdge.n2.vertex) < 0) rootEdge.n2 else rootEdge.n1
      StateManager.this.publish(new RegenerateTerrainEvent(ctSimp, pointAtInfinity))
    }
  }
  */

  val menuLookAt = new RingMenuItem("Look At") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      // Get the selection
      val selection = tcManager.componentMap.find(p => p._2.selectionStatus != 0).get._2
      val ita = selection.terrainComponent.getGeometry.asInstanceOf[TriangleStripArray]
      val itaCoords = Array.fill(ita.getVertexCount)(new Point3f)
      ita.getCoordinates(0, itaCoords)
      var itaCx = 0d
      var itaCy = 0d
      var itaCz = 0d
      itaCoords.foreach(p => {
        itaCx += p.x
        itaCy += p.y
        itaCz += p.z
      })
      itaCx /= itaCoords.size.toDouble
      itaCy /= itaCoords.size.toDouble
      itaCz /= itaCoords.size.toDouble

      terrainPanel.reactions(new SetCameraEvent(new Point3d(itaCx, itaCy, itaCz), 1d))

      val la = selection.ctComponent.getGeometry.asInstanceOf[LineArray]
      val laCoords = Array.fill(2)(new Point3f)
      la.getCoordinates(0, laCoords)
      val laCx = (laCoords(0).x + laCoords(1).x) / 2.0d
      val laCy = (laCoords(0).y + laCoords(1).y) / 2.0d
      val laCz = (laCoords(0).z + laCoords(1).z) / 2.0d
      contourTreePanel.reactions(new SetCameraEvent(new Point3d(laCx, laCy, laCz), 1d))

      var cx = 0d
      var cy = 0d
      var cz = 0d
      val pcdCoords = new scala.collection.mutable.ArrayBuffer[Array[Float]]
      pcdCoords += morseFunction.vertices(selection.ctEdge.n1.vertex)
      pcdCoords += morseFunction.vertices(selection.ctEdge.n2.vertex)
      selection.ctEdge.noncriticalNodes.foreach(n => {
        pcdCoords += morseFunction.vertices(n.vertex)
      })

      pcdCoords.foreach(p => {
        cx += p(0)
        cy += p(1)
        cz += p(2)
      })

      cx /= pcdCoords.size.toDouble
      cy /= pcdCoords.size.toDouble
      cz /= pcdCoords.size.toDouble

      //							pointCloudView.reactions(new SetCameraEvent(new Point3d(cx, cy, cz), 1d))
      pointCloudView.reactions(new SetCameraEvent(new Point3d(cx, cy, cz), 1d))
      //      triangulatedPointCloudView.reactions(new SetCameraEvent(new Point3d(cx, cy, 0), 1d))
      //      pointCloudView.reactions(new SetCameraEvent(new Point3d(pcdCoords(0)(0), pcdCoords(0)(1), pcdCoords(0)(2)), 1d))
      //      triangulatedPointCloudView.reactions(new SetCameraEvent(new Point3d(pcdCoords(0)(0), pcdCoords(0)(1), pcdCoords(0)(2)), 1d))
    }
  }

  def getJetColormap(g: ScalarFunction): AylaColormap = new JetAylaColormap(g)

  def getDivergingRedBlueColormap(g: ScalarFunction) = getDivergingColormap(g, Color.red, Color.blue)

  def getDivergingColormap(g: ScalarFunction, colorLow: Color, colorHigh: Color): AylaColormap = {
    val cmInterp = new DivergingColormap(colorLow, colorHigh)
    val gammaCmap = new DivergingAylaColormap(g, cmInterp)
    gammaCmap
  }

  sealed abstract class TopoComponentColorMode
  object LinearInterp extends TopoComponentColorMode
  object Mean extends TopoComponentColorMode
  object StdDev extends TopoComponentColorMode
  object LocalCorrelation extends TopoComponentColorMode

  var terrainPanel: TerrainPanel = null
  var contourTreePanel: ContourTreePanel = null
  var pointCloudView: PointCloudView = null
  var triangulatedPointCloudView: PointCloudView = null
  var tcManager: TopologicalComponentManager = null

  var topoComponentColorMode: TopoComponentColorMode = LinearInterp

  val menuJetColormap = new RingMenuItem("Jet") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Applying jet colormap", -1)
      colormapGenerator = getJetColormap _
      val newCmap = colormapGenerator(colorFunction)
      StateManager.this.publish(new ColormapUpdate(newCmap, colorFunction))
    }
  }
  val menuDivergingRedBlueColormap = new RingMenuItem("Diverging (red/blue)") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Applying diverging colormap", -1)
      colormapGenerator = getDivergingRedBlueColormap _
      val newCmap = colormapGenerator(colorFunction)
      StateManager.this.publish(new ColormapUpdate(newCmap, colorFunction))
    }
  }

  val menuBinaryColormap = new RingMenuItem("Binary") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Applying binary colormap", -1)
      val ctEdges = ctSimp.criticalNodeToIncidentEdges.values.flatten.toSet

      val counts = ctEdges.map(e => {
        val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
        nodeIDs.count(i => colorFunction.getFuncVal(i) != 0)
      })

      val roi = ctEdges.flatMap { e =>
        val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
        nodeIDs.filter(i => colorFunction.getFuncVal(i) != 0)
      }.toSet

      val minCount = counts.min
      val maxCount = counts.max
      val gammaCmap = new ProteinAylaColormap(minCount, maxCount, roi, colorFunction)
      StateManager.this.publish(new ColormapUpdate(gammaCmap, colorFunction))
    }
  }

  val menuCustomDivergingColormap = new RingMenuItem("Custom Diverging...") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Applying custom diverging colormap", -1)

      val c1 = JColorChooser.showDialog(null, "Choose Low Color", Color.decode("#0011AD"))
      val c2 = JColorChooser.showDialog(null, "Choose High Color", Color.decode("#FF9400"))

      colormapGenerator = getDivergingColormap(_, c1, c2)
      val newCmap = colormapGenerator(colorFunction)
      StateManager.this.publish(new ColormapUpdate(newCmap, colorFunction))
    }
  }

  /*
  val menuNtpmdColormap = new RingMenuItem("NTPMD") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Applying NTPMD colormap", -1)
      val ctEdges = ctSimp.criticalNodeToIncidentEdges.values.flatten.toSet
      val counts = ctEdges.map(e => {
        val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
        nodeIDs.count(GammaColormaps.rangeNTPMD.contains(_))
      })
      val minCount = counts.min
      val maxCount = counts.max
      val gammaCmap = new ProteinGammaColormap(minCount, maxCount, GammaColormaps.rangeNTPMD, morseFunction)
      StateManager.this.publish(new ColormapUpdate(gammaCmap, morseFunction))
    }
  }
  */

  val menuSimplifyContourTree = new RingMenuItem("Simplify Contour Tree") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Calculating persistence values", -1)
      new SimplificationDialog(ctSimp).simpThreshold match {
        case Some(threshold) => {
          println(threshold)
          println("Doing simplification")

          StateManager.this.publish(new SimplifyContourTree(threshold))

          //          val newCT = ctSimp.simplify(threshold)
          //          val newStateManager = new StateManager(dataset, morseFunction, newCT, nodeAtInfinity)
          //          val qf = new QuadFrame(newStateManager)
          //          qf.visible = true
        }
        case None => {}
      }
    }
  }

  val menuSetROI = new RingMenuItem("Set ROI") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Setting ROI", -1)

      val selectedEdge = tcManager.componentMap.find(p => p._2.selectionStatus != 0).get._1
      val cutVertex = if (ctSimp.scalarFunction.vc.compare(selectedEdge.n1.vertex, selectedEdge.n2.vertex) < 0) selectedEdge.n1.vertex else
        selectedEdge.n2.vertex

      val uf = new UnionFind(ctSimp.nodesAugmented.size)
      ctSimp.nodesAugmented.iterator.filter(_.vertex != cutVertex).foreach(child => {
        child.parents.map(parent => {
          uf.union(child.vertex, parent.vertex)
        })
      })

      println("Connected components found:")
      println(ctSimp.nodesAugmented.map(n => uf.find(n.vertex)).distinct.size)

      val cutSetID = uf.find(cutVertex)
      tcManager.componentMap.foreach { tcEntry =>
        val e = tcEntry._1
        val tc = tcEntry._2
        val rendAtts = tc.terrainComponent.getAppearance().getRenderingAttributes()
        if (uf.find(e.n1.vertex) != cutSetID) {
          rendAtts.setVisible(false)
        } else {
          rendAtts.setVisible(true)
        }
      }
    }
  }

  val menuClearROI = new RingMenuItem("Clear ROI") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Clearing ROI", -1)

      tcManager.componentMap.foreach { tcEntry =>
        val tc = tcEntry._2
        val rendAtts = tc.terrainComponent.getAppearance().getRenderingAttributes()
        rendAtts.setVisible(true)
      }
    }
  }

  val menuFindConformation = new RingMenuItem("Find...") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Getting search parameters", -1)
      Dialog.showInput(null, "Enter regular expression", "Find Conformations", initial = "") match {
        case Some(regexStr) => {
          try {
            val regex = regexStr.r
            progressListener.postProgress("Server is searching for matches...", -1)
            val searchResults = dataset.findMatchingConformations(regex)
            println("Found " + searchResults.length + " search results")
            searchResults.foreach(result => println("\t" + result))

            val frame = new ConformationSearchResultsFrame(searchResults, tcManager, conformationalSpacePanel)
          } catch {
            case e: PatternSyntaxException =>
              Dialog.showMessage(
                title = "Regex Error", message = e.getMessage())
          }
        }
        case None => { /* Do nothing */ }
      }
    }
  }

  /*
  val menuRemd20KColormap = new RingMenuItem("REMD 20K") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Applying REMD 20K colormap", -1)
      val ctEdges = ctSimp.criticalNodeToIncidentEdges.values.flatten.toSet
      val counts = ctEdges.map(e => {
        val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
        nodeIDs.count(GammaColormaps.rangeREMD.contains(_))
      })
      val minCount = counts.min
      val maxCount = counts.max
      val gammaCmap = new ProteinGammaColormap(minCount, maxCount, GammaColormaps.rangeREMD, morseFunction)
      StateManager.this.publish(new ColormapUpdate(gammaCmap, morseFunction))
    }
  }
  */

  /*
  val menuTrpSuccessColormap = new RingMenuItem("TrpCage S") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Applying TrpCage S colormap", -1)

      val pat = "-".r
      val roiMap = withBufferedReader(new File("/media/intel_ssd/traj.txt")) { br =>
        Stream.continually(br.readLine).takeWhile(_ != null).flatMap { line =>
          val lineSplit = pat.split(line)
          if (lineSplit(3).startsWith("f")) {
            Some(lineSplit(0) -> (lineSplit(1).toInt to lineSplit(2).toInt))
          } else {
            None
          }
        }.toMap
      }

      val pdbFileNames = withBufferedReader(new File("/home/harveywi/research/ScalaCrystals/TrpCage/conformation_filenames.txt")) { br =>
        val dropString = "/media/intel_ssd/TrpCage/"
        val start = dropString.length
        Iterator.continually(br.readLine).takeWhile(_ != null).map { line =>
          line.substring(start)
        }.toArray
      }

      val isSuccess = pdbFileNames.map { pdbFileName =>
        val trajID = pdbFileName.take(4)
        val trajStep = pdbFileName.substring(5, pdbFileName.length - 4).toInt
        roiMap.get(trajID) match {
          case Some(roi) => roi.contains(trajStep)
          case None => false
        }
      }

      val ctEdges = ctSimp.criticalNodeToIncidentEdges.values.flatten.toSet
      val counts = ctEdges.map { e =>
        val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
        nodeIDs.count(isSuccess)
      }

      val minCount = counts.min
      val maxCount = counts.max

      val gammaCmap = new ProteinGammaColormap(minCount, maxCount, new { def contains(i: Int) = isSuccess(i) }, morseFunction)
      StateManager.this.publish(new ColormapUpdate(gammaCmap, morseFunction))
    }
  }

  val menuTrpFailColormap = new RingMenuItem("TrpCage F") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Applying TrpCage F colormap", -1)

      val pat = "-".r
      val roiMap = withBufferedReader(new File("/media/intel_ssd/traj.txt")) { br =>
        Stream.continually(br.readLine).takeWhile(_ != null).flatMap { line =>
          val lineSplit = pat.split(line)
          if (lineSplit(3).startsWith("r")) {
            Some(lineSplit(0) -> (lineSplit(1).toInt to lineSplit(2).toInt))
          } else {
            None
          }
        }.toMap
      }

      val pdbFileNames = withBufferedReader(new File("/home/harveywi/research/ScalaCrystals/TrpCage/conformation_filenames.txt")) { br =>
        val dropString = "/media/intel_ssd/TrpCage/"
        val start = dropString.length
        Iterator.continually(br.readLine).takeWhile(_ != null).map { line =>
          line.substring(start)
        }.toArray
      }

      val isFail = pdbFileNames.map { pdbFileName =>
        val trajID = pdbFileName.take(4)
        val trajStep = pdbFileName.substring(5, pdbFileName.length - 4).toInt
        roiMap.get(trajID) match {
          case Some(roi) => roi.contains(trajStep)
          case None => false
        }
      }

      val ctEdges = ctSimp.criticalNodeToIncidentEdges.values.flatten.toSet
      val counts = ctEdges.map { e =>
        val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
        nodeIDs.count(isFail)
      }

      val minCount = counts.min
      val maxCount = counts.max

      val gammaCmap = new ProteinGammaColormap(minCount, maxCount, new { def contains(i: Int) = isFail(i) }, morseFunction)
      StateManager.this.publish(new ColormapUpdate(gammaCmap, morseFunction))
    }
  }

  val menuRemd300KColormap = new RingMenuItem("REMD 300K") {
    override def handleClick(progressListener: RingMenuProgressListener) = {
      progressListener.postProgress("Applying REMD 300K colormap", -1)
      val ctEdges = ctSimp.criticalNodeToIncidentEdges.values.flatten.toSet
      val counts = ctEdges.map(e => {
        val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
        nodeIDs.count(GammaColormaps.rangeREMD_new.contains(_))
      })
      val minCount = counts.min
      val maxCount = counts.max
      val gammaCmap = new ProteinGammaColormap(minCount, maxCount, GammaColormaps.rangeREMD_new, morseFunction)
      StateManager.this.publish(new ColormapUpdate(gammaCmap, morseFunction))
    }
  }
  */

  val awaitDatasetMenu: SimpleDirectedGraph[RingMenuItem, DefaultEdge] = {
    val menuGraph = new SimpleDirectedGraph[RingMenuItem, DefaultEdge](classOf[DefaultEdge])

    val menuAbout = new RingMenuItem("About") {
      override def handleClick(progressListener: RingMenuProgressListener) = Dialog.showMessage(
        title = "About Ayla", message = "Ayla Visual Analytics\nCopyright 2012 William Harvey\nharveywi@cse.ohio-state.edu")
    }
    val menuExit = new RingMenuItem("Exit") { override def handleClick(progressListener: RingMenuProgressListener) = System.exit(0) }

    List(menuRoot, menuAbout, menuExit).foreach(menuGraph.addVertex)
    menuGraph.addEdge(menuRoot, menuAbout)
    menuGraph.addEdge(menuRoot, menuExit)

    menuGraph
  }

  def awaitMorseFunctionMenu = {
    val menuGraph = awaitDatasetMenu.clone.asInstanceOf[SimpleDirectedGraph[RingMenuItem, DefaultEdge]]

    menuGraph
  }

  def awaitKnnGraphMenu = {
    val menuGraph = awaitMorseFunctionMenu.clone.asInstanceOf[SimpleDirectedGraph[RingMenuItem, DefaultEdge]]
    menuGraph
  }

  def awaitTauOrColorFuncMenu = {
    val menuGraph = awaitKnnGraphMenu.clone.asInstanceOf[SimpleDirectedGraph[RingMenuItem, DefaultEdge]]

    val actionMenuItem = new RingMenuItem("Action")
    menuGraph.addVertex(actionMenuItem)
    menuGraph.addEdge(menuRoot, actionMenuItem)
    List(menuLookAt, menuSimplifyContourTree, menuSetROI, menuClearROI, menuFindConformation).foreach(item => {
      menuGraph.addVertex(item)
      menuGraph.addEdge(actionMenuItem, item)
    })

    val colormapMenuItem = new RingMenuItem("Set Colormap")
    menuGraph.addVertex(colormapMenuItem)
    menuGraph.addEdge(actionMenuItem, colormapMenuItem)
    List(menuJetColormap, menuDivergingRedBlueColormap, menuCustomDivergingColormap, menuBinaryColormap).foreach(item => {
      menuGraph.addVertex(item)
      menuGraph.addEdge(colormapMenuItem, item)
    })

    val colorFunctionMenuItem = new RingMenuItem("Color Function")
    menuGraph.addVertex(colorFunctionMenuItem)
    menuGraph.addEdge(actionMenuItem, colorFunctionMenuItem)
    val parentDirMap = new scala.collection.mutable.HashMap[File, RingMenuItem]
    dataset.scalarArrays.foreach { funcFile =>
      val funcName = if (funcFile.getName.toLowerCase.endsWith(".txt")) funcFile.getName.dropRight(4) else funcFile.getName
      var mi1: RingMenuItem = new SetColorFunctionMenuItem(funcName, funcFile)
      menuGraph.addVertex(mi1)
      var parentDir = funcFile.getParentFile()
      while (parentDir.getName != "scalar_functions") {
        val mi2 = parentDirMap.getOrElseUpdate(parentDir, {
          val mi = new RingMenuItem(parentDir.getName)
          menuGraph.addVertex(mi)
          mi
        })
        menuGraph.addEdge(mi2, mi1)
        mi1 = mi2
        parentDir = parentDir.getParentFile()
      }
      menuGraph.addEdge(colorFunctionMenuItem, mi1)
    }
    //    
    //    val colorFunctionsGraph = dataset.colorFunctions
    //    val colorFunctionMenuItems = colorFunctionsGraph.vertexSet().map{case (funcName, file) =>
    //      
    //    }
    //    colorFunctionsGraph.edgeSet().foreach{e =>
    //      
    //    }
    // TODO
    //    val stack = new scala.collection.mutable.Stack[(RingMenuItem, File)]
    //    val scalarFunctionDir = new File(dataset.dir, "scalar_functions")
    //    stack.push((colorFunctionMenuItem, scalarFunctionDir))

    //    while (!stack.isEmpty) {
    //      val (dirMenuItem, dir) = stack.pop
    //
    //      dir.listFiles.foreach(file => {
    //        if (file.getName.endsWith(".txt")) {
    //          val funcName = scala.io.Source.fromFile(file).getLines.next()
    //          val menuItem = new SetColorFunctionMenuItem(funcName, file)
    //          menuGraph.addVertex(menuItem)
    //          menuGraph.addEdge(dirMenuItem, menuItem)
    //        } else if (file.isDirectory) {
    //          val mi = new RingMenuItem(file.getName)
    //          menuGraph.addVertex(mi)
    //          menuGraph.addEdge(dirMenuItem, mi)
    //          stack.push((mi, file))
    //        } else {
    //          System.err.println("Warning:  Ignoring scalar function file: " + file.getAbsolutePath)
    //        }
    //      })
    //    }

    val interpModeMenuItem = new RingMenuItem("Color Interpolation")
    val interpLinearMenuItem = new RingMenuItem("Linear") {
      override def handleClick(progressListener: RingMenuProgressListener) = {
        progressListener.postProgress("Applying linear color interpolation", -1)
        topoComponentColorMode = LinearInterp
        showSecondaryStructure(morseFunction, (0 until colorFunction.vertices.size).map(colorFunction.getFuncVal(_)).toArray, ctSimp)
      }
    }
    val interpMeanMenuItem = new RingMenuItem("Mean") {
      override def handleClick(progressListener: RingMenuProgressListener) = {
        progressListener.postProgress("Applying mean color interpolation", -1)
        topoComponentColorMode = Mean
        showSecondaryStructure(morseFunction, (0 until colorFunction.vertices.size).map(colorFunction.getFuncVal(_)).toArray, ctSimp)
      }
    }

    val interpStdevMenuItem = new RingMenuItem("Standard Deviation") {
      override def handleClick(progressListener: RingMenuProgressListener) = {
        progressListener.postProgress("Applying standard deviation color interpolation", -1)
        topoComponentColorMode = StdDev
        showSecondaryStructure(morseFunction, (0 until colorFunction.vertices.size).map(colorFunction.getFuncVal(_)).toArray, ctSimp)
      }
    }

    val interpLocalCorrelationMenuItem = new RingMenuItem("Local Correlation") {
      override def handleClick(progressListener: RingMenuProgressListener) = {
        progressListener.postProgress("Applying local correlation color interpolation", -1)
        topoComponentColorMode = LocalCorrelation
        showSecondaryStructure(morseFunction, (0 until colorFunction.vertices.size).map(colorFunction.getFuncVal(_)).toArray, ctSimp)
      }
    }

    List(interpModeMenuItem, interpLinearMenuItem, interpMeanMenuItem, interpStdevMenuItem, interpLocalCorrelationMenuItem).foreach(menuGraph.addVertex(_))
    menuGraph.addEdge(menuRoot, interpModeMenuItem)
    menuGraph.addEdge(interpModeMenuItem, interpLinearMenuItem)
    menuGraph.addEdge(interpModeMenuItem, interpMeanMenuItem)
    menuGraph.addEdge(interpModeMenuItem, interpStdevMenuItem)
    menuGraph.addEdge(interpModeMenuItem, interpLocalCorrelationMenuItem)

    menuGraph
  }

  //  def getScalarFunction(k: Int, dataset: CachedDataset, morseFuncVals: Array[Float]): ScalarFunction = {
  //    // Load 3D PCA vertices
  //    val vertices = dataset.pcaPoints.toArray
  //
  //    // Make sure the PCA data is centered
  //    val center = (for (i <- 0 until 3) yield {
  //      vertices.map(_(i)).reduceLeft(_ + _) / vertices.size.toFloat
  //    }).toArray
  //    println("center: " + center.mkString(","))
  //    for (v <- vertices; i <- 0 until 3) {
  //      v(i) -= center(i)
  //    }
  //
  //    val edgesWithDupes = (0 until dataset.neighbors.size).flatMap(i =>
  //      dataset.neighbors(i).take(k).map(j => Tuple2(math.min(i, j), math.max(i, j)))).toArray
  //
  //    val edges = edgesWithDupes.distinct.filter { case (i, j) => i != j }.map { case (i, j) => Array(i, j) }
  //
  //    return new ScalarFunction(vertices, edges, morseFuncVals)
  //  }

  def showBinaryFunction(f: ScalarFunction, gFuncVals: Array[Float], ctSimp: ContourTree): Unit = {
    val g = new ScalarFunction(f.vertices, f.faces, gFuncVals)
    val ctEdges = ctSimp.criticalNodeToIncidentEdges.values.flatten.toSet

    val counts = ctEdges.map(e => {
      val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
      nodeIDs.count(i => g.getFuncVal(i) != 0)
    })

    val roi = ctEdges.flatMap { e =>
      val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
      nodeIDs.filter(i => g.getFuncVal(i) != 0)
    }.toSet

    val minCount = counts.min
    val maxCount = counts.max
    val gammaCmap = new ProteinAylaColormap(minCount, maxCount, roi, g)
    StateManager.this.publish(new ColormapUpdate(gammaCmap, g))
  }

  def showSecondaryStructure(f: ScalarFunction, gFuncVals: Array[Float], ctSimp: ContourTree): Unit = {
    val g = new ScalarFunction(f.vertices, f.faces, gFuncVals)

    val (gammaCmap, statisticSF) = topoComponentColorMode match {
      case LinearInterp => {
        val cmapProxy = colormapGenerator(g)
        (cmapProxy, g)
      } //(new JetGammaColormap(g), g)
      case Mean => {
        // Compute mean value within each contour tree edge
        val edgeToMean = ctSimp.criticalNodeToIncidentEdges.values.flatten.map { e =>
          {
            val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
            val mean = nodeIDs.map(gFuncVals(_)).sum / nodeIDs.size.toFloat
            e -> mean
          }
        }.toMap

        val meanVals = edgeToMean.values.toArray

        val dummy = Array.fill(edgeToMean.size)(Array(0f))
        val meanSF = new ScalarFunction(dummy, Array.empty[Array[Int]], meanVals)

        val cmapProxy = colormapGenerator(meanSF)
        val cmap = new AylaColormap(meanSF) {
          override def getColor(f: Float) = cmapProxy.getColor(f)
          override def getColor(e: ContourTreeEdge, alpha: Double): Color4f = {
            val f = edgeToMean(e)
            val c = getColor(f)
            val ret = new Color4f(c)
            ret.w = 1f
            return ret
          }
        }

        (cmap, meanSF)

        //        (new JetGammaColormap(meanSF) {
        //          override def getColor(e: ContourTreeEdge, alpha: Double): Color4f = {
        //            val f = edgeToMean(e)
        //            val c = colormap.Colormaps.getColor(f, sf.minFuncVal, sf.rangeFuncVal, colormap.Colormaps.CmapType.JET)
        //            val ret = new Color4f(c)
        //            ret.w = 1f
        //            return ret
        //          }
        //        }, meanSF)

      }
      case StdDev => {
        // Compute mean value within each contour tree edge
        val edgeToStdev = ctSimp.criticalNodeToIncidentEdges.values.flatten.map { e =>
          {
            val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
            val mean = nodeIDs.map(gFuncVals(_)).sum / nodeIDs.size.toFloat

            val stdev = math.sqrt(nodeIDs.map(i => {
              val f = gFuncVals(i)
              (f - mean) * (f - mean)
            }).sum / nodeIDs.size).toFloat
            e -> stdev
          }
        }.toMap

        val stdevVals = edgeToStdev.values.toArray

        val dummy = Array.fill(edgeToStdev.size)(Array(0f))
        val stdevSF = new ScalarFunction(dummy, Array.empty[Array[Int]], stdevVals)

        (new JetAylaColormap(stdevSF) {
          override def getColor(e: ContourTreeEdge, alpha: Double): Color4f = {
            val f = edgeToStdev(e)
            val c = colormap.Colormaps.getColor(f, sf.minFuncVal, sf.rangeFuncVal, colormap.Colormaps.CmapType.JET)
            val ret = new Color4f(c)
            ret.w = 1f
            return ret
          }
        }, stdevSF)
      }
      case LocalCorrelation => {
        // Compute correlation of Morse function with the current colormap function
        val edgeToLocalCorrelation = ctSimp.criticalNodeToIncidentEdges.values.flatten.map { e =>
          val nodeIDs = List[Int](e.n1.vertex, e.n2.vertex) ::: e.noncriticalNodes.map(_.vertex)
          val n = nodeIDs.size.toFloat
          val mean1 = nodeIDs.map(ctSimp.scalarFunction.getFuncVal).sum / n
          val mean2 = nodeIDs.map(gFuncVals).sum / n
          val numerator = nodeIDs.map(i => (ctSimp.scalarFunction.getFuncVal(i) - mean1) * (gFuncVals(i) - mean2)).sum
          val sumSq1 = nodeIDs.map(i => {
            val diff = ctSimp.scalarFunction.getFuncVal(i) - mean1
            diff * diff
          }).sum
          val sumSq2 = nodeIDs.map(i => {
            val diff = gFuncVals(i) - mean2
            diff * diff
          }).sum
          val denom = math.sqrt(sumSq1 * sumSq2).toFloat
          val lc = if (denom == 0) 1f else (numerator / denom)
          e -> lc
        }.toMap

        val localCorrelationVals = edgeToLocalCorrelation.values.toArray
        println("Local correlations:  " + localCorrelationVals.distinct.mkString("", ",", "\n"))

        val dummy = Array.fill(edgeToLocalCorrelation.size)(Array(0f))
        val stdevSF = new ScalarFunction(dummy, Array.empty[Array[Int]], localCorrelationVals)

        val cmInterp = new DivergingColormap(Color.blue, Color.red)

        (new DivergingAylaColormap(stdevSF, cmInterp) {
          val minLocalCorrelation = localCorrelationVals.min
          val maxLocalCorrelation = localCorrelationVals.max
          override def getColor(e: ContourTreeEdge, alpha: Double): Color4f = {
            val f = edgeToLocalCorrelation(e)
            val c = cmInterp.interpolateDiverging((f - minLocalCorrelation) / (maxLocalCorrelation - minLocalCorrelation))
            //            val c = colormap.Colormaps.getColor(f, sf.minFuncVal, sf.rangeFuncVal, colormap.Colormaps.CmapType.JET)
            val ret = new Color4f(c)
            ret.w = 1f
            return ret
          }
        }, stdevSF)
      }
    }

    //		colorFunction = statisticSF

    StateManager.this.publish(new ColormapUpdate(gammaCmap, g))

  }

}
