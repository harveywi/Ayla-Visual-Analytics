package edu.osu.compgeom.ayla

import java.io.ByteArrayInputStream
import java.io.DataOutputStream
import java.io.File
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.net.InetAddress
import java.net.Socket
import java.util.zip.GZIPInputStream
import scala.Array.canBuildFrom
import scala.swing.Publisher
import scala.util.matching.Regex
import org.jgrapht.alg.ConnectivityInspector
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.SimpleGraph
import akka.actor.actorRef2Scala
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.dispatch._
import akka.pattern.ask
import akka.util.duration.intToDurationInt
import akka.util.Timeout
import edu.osu.compgeom.ayla.collab.AylaCollaborationFrame
import edu.osu.compgeom.ayla.collab.ConformationAnnotation
import edu.osu.compgeom.ayla.collab.ConformationAnnotationListItem
import edu.osu.compgeom.ayla.collab.Storyboard
import edu.osu.compgeom.ayla.event.AnnotationsRefreshed
import edu.osu.compgeom.ayla.event.StoryboardsRefreshed
import edu.osu.compgeom.ayla.menu.PieMenu
import edu.osu.compgeom.ayla.message._
import edu.osu.compgeom.ct.ContourTree
import edu.osu.compgeom.ct.ContourTreeNode
import edu.osu.compgeom.omegavis.DatasetExplorer
import edu.osu.compgeom.util.IO.withObjectInputStream
import edu.osu.compgeom.util.IO.withObjectOutputStream
import edu.osu.compgeom.util.Timing
import scala.util.continuations._

class AylaClient(userName: String, out: ObjectOutputStream, in: ObjectInputStream) extends Publisher {
  val collabFrame = new AylaCollaborationFrame(this)
  val actorSystem = ActorSystem("AylaServerSystem")
  val clientActor = actorSystem.actorOf(Props(new ClientActor))

  def registerUserName(): Unit = {
    implicit val timeout = Timeout(5 seconds)
    val future = clientActor ? RegisterUserNameRequest(userName)
    val result = Await.result(future, timeout.duration)
    println("Successfully logged into server using username '%s'".format(userName))
  }

  def getCollabProjectList: PieMenu.PieMenuGraph = {
    implicit val timeout = Timeout(1 minute)
    val future = clientActor ? GetCollabProjectListRequest
    val result = Await.result(future, timeout.duration).asInstanceOf[GetCollabProjectListResponse]
    result.menuGraph
  }

  def getCollabProject(projectIdentifier: (String, File), cacheDir: File): AylaCollaborationProject = {
    // Check the cache first for a matching project
    val fileInCache = new File(cacheDir, projectIdentifier._2.getParentFile.getParentFile().getName + "/" + projectIdentifier._2.getName)
    if (fileInCache.exists) {
      println("Found cached project")
      val collabProject = withObjectInputStream(fileInCache) { _.readObject.asInstanceOf[AylaCollaborationProject] }

      // Notify the server that we opened a locally-cached version of this project
      implicit val timeout = Timeout(4 minutes)
      val future = clientActor ? PostClientOpenedCachedCollabProject(projectIdentifier)
      Await.result(future, timeout.duration)

      collabProject
    } else {
      println("Project not found in cache.  Retrieving from server.")
      implicit val timeout = Timeout(10 minutes)
      val future = clientActor ? GetCollabProjectRequest(projectIdentifier)
      val result = Await.result(future, timeout.duration).asInstanceOf[GetCollabProjectResponse]
      val collabProject = result.proj

      if (!fileInCache.getParentFile().exists)
        fileInCache.getParentFile().mkdirs()
      withObjectOutputStream(fileInCache) { _.writeObject(collabProject) }
      collabProject
    }
  }

  // TODO
  def estimateAreas(ct: ContourTree): Unit = {
    val edges = ct.criticalNodeToIncidentEdges.values.flatten.toArray.distinct
    val vertBatches = edges.map(e => Array(e.n1.vertex, e.n2.vertex) ++ e.noncriticalNodes.map(_.vertex))

    implicit val timeout = Timeout(10 minutes)
    val future = clientActor ? EstimateAreasRequest(vertBatches)
    val result = Await.result(future, timeout.duration).asInstanceOf[EstimateAreasResponse]
    edges.iterator.zip(result.areas.iterator).foreach { case (e, area) => e.area = area }
  }

  def getPDBLines(i: Int): Array[String] = {
    implicit val timeout = Timeout(1 minute)
    val future = clientActor ? GetPDBRequest(i)
    val result = Await.result(future, timeout.duration).asInstanceOf[GetPDBResponse]
    val bais = new ByteArrayInputStream(result.compressedBytes)
    val gzipIn = new GZIPInputStream(bais)
    val ois = new ObjectInputStream(gzipIn)
    ois.readObject.asInstanceOf[Array[String]]
  }

  def getDsspOutput(): Option[Array[Char]] = {
    implicit val timeout = Timeout(5 minutes)
    val future = clientActor ? GetDSSPOutputRequest
    val result = Await.result(future, timeout.duration).asInstanceOf[GetDSSPOutputResponse]
    result.compressedBytesOpt match {
      case Some(bytes) => {
        val bais = new ByteArrayInputStream(bytes)
        val gzipIn = new GZIPInputStream(bais)
        val ois = new ObjectInputStream(gzipIn)
        Some(ois.readObject.asInstanceOf[Array[Char]])
      }
      case None => None
    }
  }

  def getColorFunctions(): Array[(File, String)] = {
    implicit val timeout = Timeout(1 minute)
    val future = clientActor ? GetColorFunctionsRequest
    val result = Await.result(future, timeout.duration).asInstanceOf[GetColorFunctionsResponse]
    result.functions
  }

  def getColorFunction(file: File): Array[Float] = {
    implicit val timeout = Timeout(1 minute)
    println("Sending color function request to client actor")
    val future = clientActor ? GetColorFunctionRequest(file)
    val result = Await.result(future, timeout.duration).asInstanceOf[GetColorFunctionResponse]
    println("Number of function values in color function:  " + result.function.length)
    result.function
  }

  def findMatchingConformations(regex: Regex): Array[(String, Int)] = {
    implicit val timeout = Timeout(3 minutes)
    val future = clientActor ? FindMatchingConformationsRequest(regex)
    val result = Await.result(future, timeout.duration).asInstanceOf[FindMatchingConformationsResponse]
    result.matches
  }

  def getContactDensities(residues: Array[Int]): Array[Int] = {
    implicit val timeout = Timeout(3 minutes)
    val future = clientActor ? GetSubsetContactDensitiesRequest(residues)
    val result = Await.result(future, timeout.duration).asInstanceOf[GetSubsetContactDensitiesResponse]
    result.contactDensities
  }

  def getDomainShortestPath(idStart: Int, idEnd: Int): Array[Int] = {
    implicit val timeout = Timeout(3 minutes)
    val future = clientActor ? GetConformationsAlongShortestPathRequest(idStart, idEnd)
    val result = Await.result(future, timeout.duration).asInstanceOf[GetConformationsAlongShortestPathResponse]
    result.pathConformations
  }

  def postChatMessage(text: String) = clientActor ! PostChatMessageToServer(userName, text)

  def postAnnotation(annotation: ConformationAnnotation) = clientActor ! PostAnnotationToServer(annotation)

  def postStoryboard(storyboard: Storyboard) = clientActor ! PostStoryboardToServer(storyboard)

  class ClientActor extends Actor {
    import context._

    //    // Open socket connection here
    //    val socket = IOManager(system).connect(serverName, portNumber)

    private[this] def sendOverSocket(x: Any): Unit = {
      out.writeObject(x)
      out.flush()
    }

    val socketReaderActor = actorOf(Props(new SocketReaderActor(this.self, in)))

    override def postStop(): Unit = {
      println("Finalized!!!")
    }

    private[this] lazy val receives = {
      println("Initializing receives")
      val reqResPairs = List(
        (classManifest[RegisterUserNameRequest], classManifest[RegisterUserNameResponse.type]),
        (classManifest[GetCollabProjectListRequest.type], classManifest[GetCollabProjectListResponse]),
        (classManifest[GetCollabProjectRequest], classManifest[GetCollabProjectResponse]),
        (classManifest[PostClientOpenedCachedCollabProject], classManifest[PostServerOpenedCachedCollabProject.type]),
        (classManifest[EstimateAreasRequest], classManifest[EstimateAreasResponse]),
        (classManifest[GetPDBRequest], classManifest[GetPDBResponse]),
        (classManifest[GetDSSPOutputRequest.type], classManifest[GetDSSPOutputResponse]),
        (classManifest[GetColorFunctionsRequest.type], classManifest[GetColorFunctionsResponse]),
        (classManifest[GetColorFunctionRequest], classManifest[GetColorFunctionResponse]),
        (classManifest[FindMatchingConformationsRequest], classManifest[FindMatchingConformationsResponse]),
        (classManifest[GetConformationsAlongShortestPathRequest], classManifest[GetConformationsAlongShortestPathResponse]))

      (reqResPairs.map {
        case (reqType, resType) =>
          ({
            case e1 if reqType.erasure.isInstance(e1) => {
              sendOverSocket(e1)
              val replyTo = sender
              context.become {
                case e if resType.erasure.isInstance(e) => {
                  replyTo ! e
                  context.unbecome()
                }
              }
            }
          }): Receive
      }).reduce(_ orElse _)
    }

    def receive = receives orElse {
      case e: PostChatMessageToServer => {
        out.writeObject(e)
        out.flush()
      }

      case ChatMessageFromServer(whoSentIt, text, timestamp) => {
        println("Got chat message from server:  (%s)".format(whoSentIt) + text)
        println("Timestamp is " + timestamp)
        val s = "%s [%s]> %s\n".format(whoSentIt, timestamp.toString, text)
        collabFrame.chatTextArea.append(s)
      }

      case e: PostAnnotationToServer => sendOverSocket(e)

      case RefreshAnnotationListRequest => sendOverSocket(RefreshAnnotationListRequest)

      case e: PostStoryboardToServer => sendOverSocket(e)

      case RefreshStoryboardListRequest => sendOverSocket(RefreshStoryboardListRequest)

      case RefreshAnnotationListResponse(annotations: Array[ConformationAnnotation]) => {
        // Only add annotations which we do not yet have
        val curAnnotationSet = collabFrame.annotationListView.listData.map(listItem => (listItem.annotation, listItem)).toMap
        val newListItems = annotations.map { a =>
          curAnnotationSet.get(a) match {
            case Some(listItem) => listItem
            case None => new ConformationAnnotationListItem(a)
          }
        }
        collabFrame.annotationListView.listData = newListItems
        publish(AnnotationsRefreshed(annotations))
      }

      case RefreshStoryboardListResponse(storyboardsFromServer: Array[Storyboard]) => {
        // TODO this is a horrible hack
        // Eventually Storyboards should only contain reference IDs to their annotations
        val curAnnotations = collabFrame.annotationListView.listData.map(_.annotation)
        val storyboards = storyboardsFromServer.map { storyboard =>
          {
            val localAnnotations = storyboard.annotations.map { serverAnn =>
              curAnnotations.find(a => a.name == serverAnn.name && a.timestamp == serverAnn.timestamp && a.sampledConformationID == serverAnn.sampledConformationID) match {
                case Some(ann) => ann
                case None => { throw new RuntimeException("Error:  Client doesn't have an annotation that was found in a server storyboard.") }
              }
            }
            new Storyboard(storyboard.name, localAnnotations)
          }
        }

        // TODO there is a problem here
        // The annotation objects in the storyboards that we get from the server need to be
        // wired up to local cached copies of the annotations
        collabFrame.storyboardPanel.savedStoryboardsListView.listData = storyboards
        publish(StoryboardsRefreshed(storyboards))
      }
    }
  }
}

object AylaClient {

  def main(args: Array[String]): Unit = {
    if (args.length != 0) {
      println("The program no longer takes command-line arguments")
      System.exit(0)
    }
    val loginOpt = LoginDialog.getLoginInfo()
    if (!loginOpt.isDefined)
      System.exit(0)
    val connectInfo = loginOpt.get

    val system = ActorSystem("Crap")
    implicit val ec = ExecutionContext.defaultExecutionContext(system)

    // List of promises for our progress monitor
    val stuff = new scala.collection.mutable.ArrayBuffer[(String, Promise[_])]

    def add[T](thing: (String, Promise[T])) = {
      stuff += thing

      thing._2.onComplete {
        case Right(result) => println("Finished " + thing._1)
        case Left(error) => ErrorDialog.showErrorAndQuit(new Exception(error))
      }

      thing._2
    }

    //    val hpMon = HexProgressMonitor2(system, "My Title")
    //
    //    hpMon.onStart
    //    
    //    val pClient = hpMon.add2("initializing Ayla client") {
    //      val client = connectToServer(connectInfo.server, connectInfo.username)
    //      client.registerUserName()
    //      client
    //    }
    //    
    //    val pMenuGraph: Promise[PieMenu.PieMenuGraph] = hpMon.add2("Retrieving menu graph from server") {
    //      pClient.map(_.getCollabProjectList)
    //    }
    //    
    //    val pMenu = hpMon.add2("initializing project menus") {
    //      pMenuGraph.map(new PieMenu(_))
    //    }

    val (pClient, pMenu) = HexProgressMonitor2.runTasks("Welcome to Ayla") { hpMon =>
      val pClient = hpMon.add2("initializing Ayla client") {
        val client = connectToServer(connectInfo.server, connectInfo.username)
        client.registerUserName()
        client
      }

      val pMenuGraph: Promise[PieMenu.PieMenuGraph] = hpMon.add2("Retrieving menu graph from server") {
        pClient.map(_.getCollabProjectList)
      }

      val pMenu = hpMon.add2("initializing project menus") {
        pMenuGraph.map(new PieMenu(_))
      }
      (pClient, pMenu)
    }

    pClient.zip(pMenu).map {
      case (client, pieMenu) => {
        OpenProjectDialog.chooseProject(pieMenu) match {
          case Some(projDescriptor) => {

            HexProgressMonitor2.runTasks("Starting Ayla Client") { hpMon =>
              val pProj = hpMon.add2("Retrieving collaboration project from server")(client.getCollabProject((projDescriptor.name, projDescriptor.file), connectInfo.cacheDir))

              val pCT = hpMon.add2("Computing contour tree") {
                pProj.map { proj =>
                  if (projDescriptor.file.getAbsolutePath().toLowerCase.contains("survivin")) {
                    println("This is the Survivin dataset.  Using a different simplification threshold.")
                    ContourTree(proj.sf).simplify(65)
                  } else {
                    println("Using standard persistence threshold.")
                    ContourTree(proj.sf).simplify(40)
                  }
                }
              }
              
              for (proj <- pProj; ct <- pCT) yield {
                client.estimateAreas(ct)
                
                import org.jgrapht.graph._
			            import org.jgrapht.alg._
			            import edu.osu.compgeom.ct.ContourTreeNode
			            val g = new SimpleGraph[ContourTreeNode, DefaultEdge](classOf[DefaultEdge])
			            val nodesAugmented = ct.nodesAugmented
			            nodesAugmented.foreach(v => g.addVertex(v))
			
			            nodesAugmented.foreach { n =>
			              n.parents.foreach(p => g.addEdge(n, p))
			            }
			            val ci = new ConnectivityInspector(g)
			
			            println("Connected?  " + ci.isGraphConnected())
			            println("num cc:  " + ci.connectedSets.size)
			            println("Connected?  " + ci.isGraphConnected())
			            println("Betti zero:  " + ct.scalarFunction.bettiZero)
                
			            val nodeAtInfinity = ct.nodesContracted.maxBy(n => proj.sf.getFuncVal(n.vertex))
                  val dataset = new AylaClientCachedDataset(client)
                  this.ct = ct
                  DatasetExplorer.launch(ct, nodeAtInfinity, dataset)
			            
              }
              
//              hpMon.add2("calculating topological component hypervolumes") {
//                for (client <- pClient; ct <- pCT) yield client.estimateAreas(ct) 
//              }
//              
//              hpMon.add2("performing domain connectivity sanity checks") {
//                for (ct <- pCT) yield {
//                  import org.jgrapht.graph._
//			            import org.jgrapht.alg._
//			            import edu.osu.compgeom.ct.ContourTreeNode
//			            val g = new SimpleGraph[ContourTreeNode, DefaultEdge](classOf[DefaultEdge])
//			            val nodesAugmented = ct.nodesAugmented
//			            nodesAugmented.foreach(v => g.addVertex(v))
//			
//			            nodesAugmented.foreach { n =>
//			              n.parents.foreach(p => g.addEdge(n, p))
//			            }
//			            val ci = new ConnectivityInspector(g)
//			
//			            println("Connected?  " + ci.isGraphConnected())
//			            println("num cc:  " + ci.connectedSets.size)
//			            println("Connected?  " + ci.isGraphConnected())
//			            println("Betti zero:  " + ct.scalarFunction.bettiZero)
//                }
//              }
              
//              val pDataset = hpMon.add2("preparing cached dataset for launch") {
//                for (ct <- pCT; proj <- pProj) yield {
//                  val nodeAtInfinity = ct.nodesContracted.maxBy(n => proj.sf.getFuncVal(n.vertex))
//                  val dataset = new AylaClientCachedDataset(client)
//                  this.ct = ct
//                  DatasetExplorer.launch(ct, nodeAtInfinity, dataset)
//                }
//              }
            }
            
//            val proj = client.getCollabProject((projDescriptor.name, projDescriptor.file), connectInfo.cacheDir)
//            val ct = if (projDescriptor.file.getAbsolutePath().toLowerCase.contains("survivin")) {
//              println("This is the Survivin dataset.  Using a different simplification threshold.")
//              ContourTree(proj.sf).simplify(65)
//            } else {
//              println("Using standard persistence threshold.")
//              ContourTree(proj.sf).simplify(40)
//            }

//            client.estimateAreas(ct)

//            import org.jgrapht.graph._
//            import org.jgrapht.alg._
//            import edu.osu.compgeom.ct.ContourTreeNode
//            val g = new SimpleGraph[ContourTreeNode, DefaultEdge](classOf[DefaultEdge])
//            val nodesAugmented = ct.nodesAugmented
//            nodesAugmented.foreach(v => g.addVertex(v))
//
//            nodesAugmented.foreach { n =>
//              n.parents.foreach(p => g.addEdge(n, p))
//            }
//            val ci = new ConnectivityInspector(g)
//
//            println("Connected?  " + ci.isGraphConnected())
//            println("num cc:  " + ci.connectedSets.size)
//            println("Connected?  " + ci.isGraphConnected())
//            println("Betti zero:  " + ct.scalarFunction.bettiZero)
//
//            val nodeAtInfinity = ct.nodesContracted.maxBy(n => proj.sf.getFuncVal(n.vertex))
//
//            val dataset = new AylaClientCachedDataset(client)
//            DatasetExplorer.launch(ct, nodeAtInfinity, dataset)
          }
          case None => System.exit(0)
        }
      }
    }

    //    pClient.success(connectToServer(connectInfo.server, connectInfo.username)).flatMap { client =>
    //      client.registerUserName()
    //      pMenuGraph.success(client.getCollabProjectList).flatMap { menuGraph =>
    //        pMenu.success(new PieMenu(menuGraph)).map { (client, _) }
    //      }
    //    }.map {
    //      case (client, pieMenu) =>
    //        OpenProjectDialog.chooseProject(pieMenu) match {
    //          case Some(projDescriptor) => {
    //            val proj = client.getCollabProject((projDescriptor.name, projDescriptor.file), connectInfo.cacheDir)
    //            val ct = if (projDescriptor.file.getAbsolutePath().toLowerCase.contains("survivin")) {
    //              println("This is the Survivin dataset.  Using a different simplification threshold.")
    //              ContourTree(proj.sf).simplify(65)
    //            } else {
    //              println("Using standard persistence threshold.")
    //              ContourTree(proj.sf).simplify(40)
    //            }
    //
    //            client.estimateAreas(ct)
    //
    //            import org.jgrapht.graph._
    //            import org.jgrapht.alg._
    //            import edu.osu.compgeom.ct.ContourTreeNode
    //            val g = new SimpleGraph[ContourTreeNode, DefaultEdge](classOf[DefaultEdge])
    //            val nodesAugmented = ct.nodesAugmented
    //            nodesAugmented.foreach(v => g.addVertex(v))
    //
    //            nodesAugmented.foreach { n =>
    //              n.parents.foreach(p => g.addEdge(n, p))
    //            }
    //            val ci = new ConnectivityInspector(g)
    //
    //            println("Connected?  " + ci.isGraphConnected())
    //            println("num cc:  " + ci.connectedSets.size)
    //            println("Connected?  " + ci.isGraphConnected())
    //            println("Betti zero:  " + ct.scalarFunction.bettiZero)
    //
    //            val nodeAtInfinity = ct.nodesContracted.maxBy(n => proj.sf.getFuncVal(n.vertex))
    //
    //            val dataset = new AylaClientCachedDataset(client)
    //            DatasetExplorer.launch(ct, nodeAtInfinity, dataset)
    //          }
    //          case None => System.exit(0)
    //        }
    //    }

    //    for {
    //      client <- pClient.success(connectToServer(connectInfo.server, connectInfo.username));
    //      client.registerUserName();
    //      menuGraph <- pMenuGraph.success(client.getCollabProjectList)
    //    } yield { 42 }

    //    pClient completeWith Future {
    //      connectToServer(connectInfo.server, connectInfo.username)
    //    } andThen {
    //      case Right(client) => {
    //        client.registerUserName()
    //
    //        pMenuGraph.success(client.getCollabProjectList).foreach { menuGraph =>
    //          pMenu.success(new PieMenu(menuGraph)).foreach{ pieMenu =>
    //            OpenProjectDialog.chooseProject(pieMenu) match {
    //            case Some(projDescriptor) => {
    //              val proj = client.getCollabProject((projDescriptor.name, projDescriptor.file), connectInfo.cacheDir)
    //              val ct = if (projDescriptor.file.getAbsolutePath().toLowerCase.contains("survivin")) {
    //                println("This is the Survivin dataset.  Using a different simplification threshold.")
    //                ContourTree(proj.sf).simplify(65)
    //              } else {
    //                println("Using standard persistence threshold.")
    //                ContourTree(proj.sf).simplify(40)
    //              }
    //
    //              client.estimateAreas(ct)
    //
    //              import org.jgrapht.graph._
    //              import org.jgrapht.alg._
    //              import edu.osu.compgeom.ct.ContourTreeNode
    //              val g = new SimpleGraph[ContourTreeNode, DefaultEdge](classOf[DefaultEdge])
    //              val nodesAugmented = ct.nodesAugmented
    //              nodesAugmented.foreach(v => g.addVertex(v))
    //
    //              nodesAugmented.foreach { n =>
    //                n.parents.foreach(p => g.addEdge(n, p))
    //              }
    //              val ci = new ConnectivityInspector(g)
    //
    //              println("Connected?  " + ci.isGraphConnected())
    //              println("num cc:  " + ci.connectedSets.size)
    //              println("Connected?  " + ci.isGraphConnected())
    //              println("Betti zero:  " + ct.scalarFunction.bettiZero)
    //
    //              val nodeAtInfinity = ct.nodesContracted.maxBy(n => proj.sf.getFuncVal(n.vertex))
    //
    //              val dataset = new AylaClientCachedDataset(client)
    //              DatasetExplorer.launch(ct, nodeAtInfinity, dataset)
    //            }
    //            case None => {
    //              System.exit(0)
    //            }
    //          }
    //          }
    //        }
    //      }
    //      case Left(error) => {
    //        ErrorDialog.showErrorAndQuit(new Exception(error))
    //      }
    //    }

    println("Whoa")
  }
  
  var ct: ContourTree = null

  def connectToServer(serverName: String, userName: String): AylaClient = {
    val ia = InetAddress.getByName(serverName)
    val socket = new Socket(ia, 9010)
    val out = new ObjectOutputStream(new DataOutputStream(socket.getOutputStream()))
    val in = new ObjectInputStream(socket.getInputStream())
    new AylaClient(userName, out, in)
    //        new AylaClient(userName, serverName, 9010)
  }
}