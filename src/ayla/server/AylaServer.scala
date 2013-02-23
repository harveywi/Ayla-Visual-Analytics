/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.server

import scalaz.{ Validation, Success, Failure }
import scalaz.Validation._
import java.io._
import ayla.dataset.CachedDataset
import akka.actor.ActorSystem
import akka.actor.Props
import scala.collection._
import java.net._
import org.jgrapht.graph._
import ayla.util.IO.{withDataInputStream, withDataOutputStream}
import ayla.collab.ConformationAnnotation
import akka.actor.ActorRef
import ayla.protocol.RefreshAnnotationsRequest
import java.util.Date
import ayla.protocol.RefreshChatLogRequest
import ayla.collab.Storyboard
import ayla.protocol.RefreshStoryboardsRequest
import ayla.pickling2.Pickling._
import shapeless._
import ayla.pickling2.DefaultPicklers._
import ayla.pickling2.DefaultUnpicklers._
import ayla.pickling2.PicklerRegistry2
import ayla.pickling2.Picklable
import ayla.pickling2.Pickler

case class ProjInfo(datasetName: String, projName: String, sfName: String) extends Picklable{
	def pickled(daos: java.io.DataOutputStream) = ProjInfo.pickler.pickle(this, daos)
}

object ProjInfo {
  implicit def iso = Iso.hlist(apply _, unapply _)
  val (pickler, unpickler) = picklerUnpickler[ProjInfo].create()
}

case class UserSession(
  username: String,
  projInfo: ProjInfo,
  dataset: CachedDataset,
  collabProject: CollaborationProject) {
  lazy val domainGraph: SimpleWeightedGraph[Int, DefaultWeightedEdge] = {
    // Build weighted graph from simplicial complex, where weights are edge lengths
    val domainGraph = new SimpleWeightedGraph[Int, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    collabProject.sf.vertices.indices.foreach(domainGraph.addVertex)
    collabProject.sf.faces.foreach { e =>
      val p1 = dataset.origPoints(collabProject.sampledToUnsampled(e(0)))
      val p2 = dataset.origPoints(collabProject.sampledToUnsampled(e(1)))
      val dist = math.sqrt(euclidDistSq(p1, p2))
      val graphEdge = domainGraph.addEdge(e(0), e(1))
      domainGraph.setEdgeWeight(graphEdge, dist)
    }
    domainGraph
  }

  @inline
  private[this] final def euclidDistSq(p1: Array[Float], p2: Array[Float]): Double = {
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
}

class AylaServer(val datasetsRootDir: File) {
  val actorSystem = ActorSystem("AylaServerSystem")

  // Mutable state junk
  // TODO this should be synchronized
  val userSessions = new mutable.HashSet[UserSession]
  val usernameToServerActor = new mutable.HashMap[String, ActorRef]

  val annotationMap: mutable.HashMap[ProjInfo, mutable.ArrayBuffer[ConformationAnnotation]] = {
    val annotationFile = new File(datasetsRootDir, "annotations.dat")
    if (annotationFile.exists) {
//      withObjectInputStream(annotationFile)(_.readObject.asInstanceOf[mutable.HashMap[ProjInfo, mutable.ArrayBuffer[ConformationAnnotation]]])
      withDataInputStream(annotationFile){dais =>
        implicit val up1 = ProjInfo.unpickler
        implicit val up2 = ConformationAnnotation.unpickler
        val m = mapUnpickler[ProjInfo, Array[ConformationAnnotation]].unpickle(dais).mapValues(_.to[mutable.ArrayBuffer])
        val ret = mutable.HashMap.empty[ProjInfo, mutable.ArrayBuffer[ConformationAnnotation]]
        m.foreach(entry => ret += entry)
        ret
      }
    } else {
      new mutable.HashMap[ProjInfo, mutable.ArrayBuffer[ConformationAnnotation]]
    }
  }

  val chatMap: mutable.HashMap[ProjInfo, mutable.ArrayBuffer[String]] = {
    val chatFile = new File(datasetsRootDir, "chatLog.dat")
    if (chatFile.exists) {
      withDataInputStream(chatFile){dais =>
        implicit val up = ProjInfo.unpickler
        val m = mapUnpickler[ProjInfo, Array[String]].unpickle(dais).mapValues(_.to[mutable.ArrayBuffer])
        val ret = mutable.HashMap.empty[ProjInfo, mutable.ArrayBuffer[String]]
        m.foreach(entry => ret += entry)
        ret
      }
    } else {
      mutable.HashMap.empty[ProjInfo, mutable.ArrayBuffer[String]]
    }
  }
  
  val storyboardMap: mutable.HashMap[ProjInfo, mutable.ArrayBuffer[Storyboard]] = {
    val storyboardFile = new File(datasetsRootDir, "storyboards.dat")
    if (storyboardFile.exists) {
      withDataInputStream(storyboardFile){dais =>
        implicit val up1 = ProjInfo.unpickler
        implicit val up2 = Storyboard.unpickler
        implicit val up3 = ConformationAnnotation.unpickler
        val ret = mutable.HashMap.empty[ProjInfo, mutable.ArrayBuffer[Storyboard]]
        val m = mapUnpickler[ProjInfo, Array[Storyboard]].unpickle(dais).mapValues(_.to[mutable.ArrayBuffer])
        m.foreach(entry => ret += entry)
        ret
      }
//      withObjectInputStream(storyboardFile)(_.readObject.asInstanceOf[mutable.HashMap[ProjInfo, mutable.ArrayBuffer[Storyboard]]])
    } else {
      new mutable.HashMap[ProjInfo, mutable.ArrayBuffer[Storyboard]]
    }
  }

  val datasets = (for {
    datasetDir <- datasetsRootDir.listFiles if datasetDir.isDirectory
  } yield {
    val collabProjectsFile = new File(datasetDir, "collab_projects")
    datasetDir.getName -> collabProjectsFile.listFiles.map(_.getName)
  }).toMap

  val scalarFunctions = (for (datasetDir <- datasetsRootDir.listFiles if datasetDir.isDirectory) yield {
    val scalarFunctionsFile = new File(datasetDir, "scalar_functions")
    datasetDir.getName -> scalarFunctionsFile.listFiles.map(_.getName)
  }).toMap

  def logAnnotation(username: String, annotation: ConformationAnnotation) = annotationMap.synchronized {
    userSessions.find(_.username == username) match {
      case Some(session) =>
        val annotations = annotationMap.getOrElseUpdate(session.projInfo, new mutable.ArrayBuffer[ConformationAnnotation])
        annotations += annotation
        withDataOutputStream(new File(datasetsRootDir, "annotations.dat")){daos =>
          implicit val p1 = ConformationAnnotation.pickler
          implicit val p2 = ProjInfo.pickler
          val am = annotationMap.map{case (key, value) =>
            key -> value.toArray
          }.toMap
          pickled(am, daos)
        }
        userSessions.filter(_.projInfo == session.projInfo).foreach { session =>
          val actor = usernameToServerActor(session.username)
          actor ! RefreshAnnotationsRequest(session.username)
        }
      case None => throw new RuntimeException
    }
  }

  def logChatMessage(username: String, message: String) = chatMap.synchronized {
    userSessions.find(_.username == username) match {
      case Some(session) =>
        val chats = chatMap.getOrElseUpdate(session.projInfo, new mutable.ArrayBuffer[String])
        chats += s"[${new Date().toString}] ${username} > ${message}"
        withDataOutputStream(new File(datasetsRootDir, "chatLot.dat")){daos =>
          implicit val p = ProjInfo.pickler
          val m = chatMap.map{case (key, value) =>
            key -> value.toArray
          }.toMap
          pickled(m, daos)
        }
//        withObjectOutputStream(new File(datasetsRootDir, "chatLog.dat"))(_.writeObject(chatMap))
        userSessions.filter(_.projInfo == session.projInfo).foreach { session =>
          val actor = usernameToServerActor(session.username)
          actor ! RefreshChatLogRequest(session.username)
        }
      case None => throw new RuntimeException
    }
  }
  
  def logStoryboard(username: String, storyboard: Storyboard) = storyboardMap.synchronized {
    userSessions.find(_.username == username) match {
      case Some(session) =>
        val storyboards = storyboardMap.getOrElseUpdate(session.projInfo, new mutable.ArrayBuffer[Storyboard])
        storyboards += storyboard
//        withObjectOutputStream(new File(datasetsRootDir, "storyboards.dat"))(_.writeObject(storyboardMap))
        withDataOutputStream(new File(datasetsRootDir, "storyboards.dat")){daos =>
          implicit val p1 = Storyboard.pickler
          implicit val p2 = ConformationAnnotation.pickler
          implicit val p3 = ProjInfo.pickler
          
          val m = storyboardMap.map{case (key, value) =>
            key -> value.toArray
          }.toMap
          pickled(m, daos)
//          implicit val p1 = Storyboard.pickler
//          implicit val p2 = ConformationAnnotation.iso
//          implicit val p3 = ConformationAnnotation.iso
//          implicit val p4 = ConformationAnnotation.pickler
//          pickled(storyboards.toArray, daos)
        }
        userSessions.filter(_.projInfo == session.projInfo).foreach{session =>
          val actor = usernameToServerActor(session.username)
          actor ! RefreshStoryboardsRequest(session.username)
        }
      case None => throw new RuntimeException
    }
  }

  try {
    val listener = new ServerSocket(9010)
    while (true) {
      val socket = listener.accept()
      val serverActor = actorSystem.actorOf(Props(new ServerActor(this, socket)))
    }
    listener.close()
  } catch {
    case e: IOException =>
      System.err.println("Could not listen on port: 9010.");
      System.exit(-1)
  }

  def estimateArea(vertIndices: Array[Int], dataset: CachedDataset): Double = {
    def calcMean(): Array[Double] = {
      val nDims = dataset.origPoints(0).size
      val mean = new Array[Double](nDims)
      vertIndices.foreach { idx =>
        val p = dataset.origPoints(idx)
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
        val p = dataset.origPoints(idx)
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

object AylaServer {
  def main(args: Array[String]): Unit = parseArgs(args).fold(errorMsg => println(errorMsg), runServer(_))
  def parseArgs(args: Array[String]): Validation[String, File] = {
    val usage = "Usage:  aylaServer datasets_root_dir"

    def parseDatasetRootDir(arg: String) = new File(arg) match {
      case f if f.exists && f.isDirectory => Success(f)
      case f @ _ => Failure(s"Error: Invalid dataset root directory ${f.getAbsolutePath}")
    }

    args match {
      case Array(datasetsRootDirArg) => parseDatasetRootDir(datasetsRootDirArg)
      case _ => Failure(usage)
    }
  }

  def runServer(datasetsRootDir: File) = new AylaServer(datasetsRootDir)
}
