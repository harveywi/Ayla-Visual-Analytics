package edu.osu.compgeom.ayla

import java.io._
import java.net.{ InetAddress, ServerSocket, Socket, SocketException }
import java.util.Random
import edu.osu.compgeom.util.IO._
import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorSystem
import edu.osu.compgeom.ayla.message._
import edu.osu.compgeom.dataset.CachedDataset
import java.util.zip.GZIPOutputStream
import java.util.Date
import edu.osu.compgeom.ayla.menu._
import akka.actor.ActorRef
import akka.util.Timeout
import akka.util.duration._
import akka.pattern.ask
import akka.dispatch.Await
import scala.collection.mutable.SynchronizedBuffer
import org.jgrapht.graph.SimpleDirectedGraph
import org.jgrapht.graph.DefaultEdge
import edu.osu.compgeom.ayla.collab.{ ConformationAnnotation, Storyboard }
import edu.osu.compgeom.dataset.{ ZipFilePdbStreamProvider, FilePdbStreamProvider }
import edu.osu.compgeom.util.Timing
import org.jgrapht.graph.SimpleWeightedGraph
import org.jgrapht.graph.DefaultWeightedEdge
import org.jgrapht.alg.DijkstraShortestPath
import scala.collection.JavaConverters._

object AylaServer {
  def main(args: Array[String]): Unit = {
    if (args.length == 0 || args.length > 2) {
      println("arguments:  ayla_dataset_root_dir [zip]")
      System.exit(0)
    }

    val rootDir = new File(args(0))
    require(rootDir.exists)

    val system = ActorSystem("AylaServerSystem")

    val useZipFilePdbStreamProvider = (args.length == 2 && args(1) == "zip")

    val collabManager = new CollaborationManager(rootDir, useZipFilePdbStreamProvider)

    // Start accepting client connections and creating server threads for each of them
    try {
      val listener = new ServerSocket(9010);
      while (true) {
        val socket = listener.accept()
        val serverActor = system.actorOf(Props(new ServerActor(socket, collabManager)))
      }
      listener.close()
    } catch {
      case e: IOException =>
        System.err.println("Could not listen on port: 9010.");
        System.exit(-1)
    }
  }

  /**
   * Manages opened datasets (ensuring that only 1 copy of the dataset is open at a time,
   * and manages message exchange among the ServerActors.
   */
  class CollaborationManager(rootDir: File, useZipPdbProvider: Boolean) {
    // Get list of possible landscapes that clients can visualize, and 
    // show who is connected to them
    val getAllCollabProjects = rootDir.listFiles.filter(_.isDirectory()).flatMap { datasetDir =>
      datasetDir.listFiles.filter(f => f.isDirectory() && f.getName == "collab_projects").flatMap { landscapeDir =>
        landscapeDir.listFiles.filter(_.getName().endsWith(".dat")).flatMap(f => {
          Timing("Reading some project") {
            withObjectInputStream(f) { ois =>
              Some(ois.readObject().asInstanceOf[AylaCollaborationProject])
            }
          }
        })
      }
    }

    getAllCollabProjects.foreach(proj => println(proj.description))

    def getProjectMenuGraph = {
      val g = PieMenu.newGraph()
      val projectGroups = getAllCollabProjects.groupBy(project => project.file.getParentFile().getParentFile())
      projectGroups.foreach {
        case (file, projects) => {
          val groupMenuItem = new PieMenuItem(file.getName(), Some(file))
          g.addVertex(groupMenuItem)
          g.addEdge(PieMenuRoot, groupMenuItem)
          projects.foreach { project =>
            val projectMenuItem = new PieMenuItem(project.name, Some(project.getDescriptor))
            g.addVertex(projectMenuItem)
            g.addEdge(groupMenuItem, projectMenuItem)
          }
        }
      }
      g
    }

    private val loadedDatasets = new scala.collection.mutable.ArrayBuffer[CachedDataset] with SynchronizedBuffer[CachedDataset]

    private val collabProjectMap = new scala.collection.mutable.HashMap[AylaCollaborationProject, ServerActor]

    private val chatLog = new BufferedWriter(new FileWriter(new File(rootDir, "chatLog.txt"), true))

    def getMatcingProject(projName: String, projFile: File) = {
      getAllCollabProjects.find(proj => proj.name == projName && proj.file == projFile).get
    }

    private val annotationMap = {
      val f = new File(rootDir, "annotations.dat")
      if (f.exists) {
        println("Loading existing annotations")
        withObjectInputStream(f)(_.readObject.asInstanceOf[scala.collection.mutable.HashMap[File, scala.collection.mutable.ArrayBuffer[ConformationAnnotation]]])
      } else {
        new scala.collection.mutable.HashMap[File, scala.collection.mutable.ArrayBuffer[ConformationAnnotation]]
      }
    }

    private val storyboardMap = {
      val f = new File(rootDir, "storyboards.dat")
      if (f.exists) {
        println("Loading existing storyboards")
        withObjectInputStream(f)(_.readObject.asInstanceOf[scala.collection.mutable.HashMap[File, scala.collection.mutable.ArrayBuffer[Storyboard]]])
      } else {
        new scala.collection.mutable.HashMap[File, scala.collection.mutable.ArrayBuffer[Storyboard]]
      }
    }

    def getCachedDataset(datasetDir: File): CachedDataset = synchronized {
      loadedDatasets.find(_.dir == datasetDir) match {
        case Some(cachedDataset) => {
          println("Already have dataset in cache")
          cachedDataset
        }
        case None => {
          val ret = try {
            new CachedDataset(datasetDir) with ZipFilePdbStreamProvider
          } catch {
            case e: FileNotFoundException => new CachedDataset(datasetDir) with FilePdbStreamProvider
          }
       
          loadedDatasets += ret
          ret
        }
      }
    }

    def logChatMessage(proj: AylaCollaborationProject, userName: String, text: String, timestamp: Date) = synchronized {
      val s = List(timestamp.toString, proj.file.getAbsolutePath(), userName, text).mkString("", "\t", "\n")
      chatLog.write(s)
      chatLog.flush()
    }

    def logAnnotation(proj: AylaCollaborationProject, annotation: ConformationAnnotation): Unit = synchronized {
      println("Logging annotation:  " + proj + "\t" + annotation.name)

      val list = annotationMap.getOrElseUpdate(proj.file, new scala.collection.mutable.ArrayBuffer[ConformationAnnotation])
      list += annotation

      // For now, just persist the annotation map each time
      withObjectOutputStream(new File(rootDir, "annotations.dat"))(_.writeObject(annotationMap))
    }

    def getAnnotations(proj: AylaCollaborationProject): Array[ConformationAnnotation] = synchronized {
      val annotationArray = annotationMap.get(proj.file) match {
        case Some(arrayBuffer) => arrayBuffer.toArray
        case None => Array.empty[ConformationAnnotation]
      }
      annotationArray.sortBy(_.timestamp)
    }

    def logStoryboard(proj: AylaCollaborationProject, storyboard: Storyboard) = synchronized {
      val list = storyboardMap.getOrElseUpdate(proj.file, new scala.collection.mutable.ArrayBuffer[Storyboard])
      list += storyboard

      // For now, just persist the storyboard map each time
      withObjectOutputStream(new File(rootDir, "storyboards.dat"))(_.writeObject(storyboardMap))
    }

    def getStoryboards(proj: AylaCollaborationProject): Array[Storyboard] = synchronized {
      val storyboardArray = storyboardMap.get(proj.file) match {
        case Some(arrayBuffer) => arrayBuffer.toArray
        case None => Array.empty[Storyboard]
      }
      storyboardArray.sortBy(_.timestamp)
    }
  }

  class ServerActor(socket: Socket, collabManager: CollaborationManager) extends Actor {
    import context._
    val out = new ObjectOutputStream(socket.getOutputStream());
    val in = new ObjectInputStream(new DataInputStream(socket.getInputStream()));

    val socketReaderActor = actorOf(Props(new SocketReaderActor(this.self, in)))

    var proj: AylaCollaborationProject = null
    var dataset: CachedDataset = null
    lazy val domainGraph: SimpleWeightedGraph[Int, DefaultWeightedEdge] = {
      // Build weighted graph from simplicial complex, where weights are edge lengths
      val domainGraph = new SimpleWeightedGraph[Int, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
      proj.sf.vertices.indices.foreach(domainGraph.addVertex)
      proj.sf.faces.filter(e => e(0) != e(1)).foreach { e =>
        val p1 = dataset.pcaPoints(proj.sampledToUnsampled(e(0)))
        val p2 = dataset.pcaPoints(proj.sampledToUnsampled(e(1)))
        if (!domainGraph.containsEdge(e(0), e(1))) {
          val dist = math.sqrt(euclidDistSq(p1, p2))
          val graphEdge = domainGraph.addEdge(e(0), e(1))
          domainGraph.setEdgeWeight(graphEdge, dist)
        }
      }
      domainGraph
    }

    // Oh, the code smell...
    def receive = {
      case RegisterUserNameRequest(userName) => {
        println("Registering username:  " + userName)
        out.writeObject(RegisterUserNameResponse)
        out.flush()
      }

      case GetCollabProjectListRequest => {
        val response = GetCollabProjectListResponse(collabManager.getProjectMenuGraph)
        out.writeObject(response)
        out.flush()
      }

      case GetCollabProjectRequest((projName, projFile)) => {
        proj = collabManager.getMatcingProject(projName, projFile) //allCollabProjects.find(proj => proj.name == projName && proj.file == projFile).get
        dataset = collabManager.getCachedDataset(proj.file.getParentFile.getParentFile) //new CachedDataset(proj.file.getParentFile.getParentFile)
        val response = GetCollabProjectResponse(proj)
        out.writeObject(response)
        out.flush()
      }

      case PostClientOpenedCachedCollabProject((projName, projFile)) => {
        proj = collabManager.getMatcingProject(projName, projFile)
        dataset = collabManager.getCachedDataset(proj.file.getParentFile.getParentFile)
        out.writeObject(PostServerOpenedCachedCollabProject)
        out.flush()
      }

      case EstimateAreasRequest(vertBatchesSampled) => {
        // The vertIndices array here is sampled vertices.  Need to convert these indices
        // to unsampled.
        println("Server is estimating areas.")
        val areas = vertBatchesSampled.map { vertBatchSampled =>
          val vertIndices = vertBatchSampled.map(proj.sampledToUnsampled)
          estimateArea(vertIndices)
        }

        val response = EstimateAreasResponse(areas)
        out.writeObject(response)
        out.flush()
      }

      case GetPDBRequest(iSampled) => {
        val pdbLines = dataset.getPDBLines(proj.sampledToUnsampled(iSampled))

        val baos = new ByteArrayOutputStream
        val gzipOut = new GZIPOutputStream(baos)
        val oos = new ObjectOutputStream(gzipOut)
        oos.writeObject(pdbLines)
        oos.close
        val bytes = baos.toByteArray
        val response = GetPDBResponse(bytes)

        out.writeObject(response)
        out.flush()
      }

      case GetDSSPOutputRequest => {
        val response = dataset.dsspOutput match {
          case Some(dsspArray) => {
            val baos = new ByteArrayOutputStream
            val gzipOut = new GZIPOutputStream(baos)
            val oos = new ObjectOutputStream(gzipOut)
            oos.writeObject(dsspArray)
            oos.close
            val bytes = baos.toByteArray
            GetDSSPOutputResponse(Some(bytes))
          }
          case None => GetDSSPOutputResponse(None)
        }
        out.writeObject(response)
        out.flush()
      }

      case GetColorFunctionsRequest => {
        val response = GetColorFunctionsResponse(dataset.colorFunctions)
        out.writeObject(response)
        out.flush()
      }

      case GetColorFunctionRequest(file) => {
        val allFuncVals = dataset.getColorFunction(file)
        val sampledFuncVals = proj.sampledToUnsampled.map(allFuncVals)
        val response = GetColorFunctionResponse(sampledFuncVals)
        out.writeObject(response)
        out.flush
      }

      case FindMatchingConformationsRequest(regex) => {
        // This finds all matches in the unsampled dataset.
        val resultUnsampled = dataset.findMatchingConformations(regex).map(_.swap).toMap
        val matchResults = proj.sampledToUnsampled.zipWithIndex.flatMap {
          case (u, s) =>
            resultUnsampled.get(u) match {
              case Some(str) => { Some(str, s) }
              case None => None
            }
        }.toArray
        val response = FindMatchingConformationsResponse(matchResults)
        out.writeObject(response)
        out.flush
      }

      case GetSubsetContactDensitiesRequest(residues) => {
        val densities = proj.sampledToUnsampled.par.map(i => dataset.getContactDensity(i, residues)).toArray
        val response = GetSubsetContactDensitiesResponse(densities)
        out.writeObject(response)
        out.flush
      }

      case GetConformationsAlongShortestPathRequest(idStart, idEnd) => {

        val dsp = new DijkstraShortestPath(domainGraph, idStart, idEnd)
        val edgeList = dsp.getPathEdgeList()
        val pathVerts = new scala.collection.mutable.ArrayBuffer[Int]
        pathVerts += idStart
        edgeList.asScala.foreach { e =>
          val v1 = domainGraph.getEdgeSource(e)
          val v2 = domainGraph.getEdgeTarget(e)
          if (pathVerts.last == v1) {
            pathVerts += v2
          } else {
            pathVerts += v1
          }
//          pathVerts += domainGraph.getEdgeTarget(e)
        }
        
        val response = GetConformationsAlongShortestPathResponse(pathVerts.toArray)
        out.writeObject(response)
        out.flush
      }

      case PostChatMessageToServer(userName, text) => {
        println("Server received a chat message:  (%s)".format(userName) + text)
        val timestamp = new Date()
        collabManager.logChatMessage(proj, userName, text, timestamp)
        context.actorSelection("../*") ! ChatMessageFromServer(userName, text, timestamp)
      }

      case e: ChatMessageFromServer => {
        out.writeObject(e)
        out.flush()
      }

      case PostAnnotationToServer(annotation) => {
        println("Server received an annotation")
        collabManager.logAnnotation(proj, annotation)
        context.actorSelection("../*") ! RefreshAnnotationListRequest
      }

      case RefreshAnnotationListRequest => {
        val annotations = collabManager.getAnnotations(proj)
        out.writeObject(RefreshAnnotationListResponse(annotations))
        out.flush()
      }

      case PostStoryboardToServer(storyboard) => {
        println("Server received a storyboard")
        collabManager.logStoryboard(proj, storyboard)
        context.actorSelection("../*") ! RefreshStoryboardListRequest
      }

      case RefreshStoryboardListRequest => {
        val storyboards = collabManager.getStoryboards(proj)
        out.writeObject(RefreshStoryboardListResponse(storyboards))
        out.flush()
      }

    }

    @inline
    final def euclidDistSq(p1: Array[Float], p2: Array[Float]): Double = {
      var sum = 0d
      var i = 0
      val n = p1.length
      while (i < n) {
        val diff = p1(i) - p2(i)
        sum += diff * diff
        i += 1
      }
      sum
    }

    def estimateArea(vertIndices: Array[Int]): Double = {
      //      if (true)
      //        return vertIndices.size
      //        if (config.useCountForAreaEstimation)
      //          return vertIndices.size
      def calcMean(): Array[Double] = {
        val nDims = dataset.pcaPoints(0).size
        val mean = new Array[Double](nDims)
        vertIndices.foreach { idx =>
          val p = dataset.pcaPoints(idx)
          (0 until nDims).foreach { i =>
            mean(i) += p(i)
          }
        }
        val n = vertIndices.size.toDouble
        (0 until nDims).foreach { i => mean(i) /= n }
        mean
      }

      def calcDistSum(mean: Array[Double]): Double = {
        var distSum = 0d
        vertIndices.foreach { idx =>
          var sumSq = 0d
          val p = dataset.pcaPoints(idx)
          (0 until mean.size).foreach { i =>
            val diff = p(i) - mean(i)
            sumSq += diff * diff
          }
          distSum += math.sqrt(sumSq)
        }
        distSum
      }

      // Compute standard deviation.
      val mean = calcMean()
      val distSum = calcDistSum(mean)

      val stdDev = distSum / (vertIndices.size - 1).toDouble
      if (stdDev <= 0) {
        return .1
      }
      return stdDev
    }
  }
}