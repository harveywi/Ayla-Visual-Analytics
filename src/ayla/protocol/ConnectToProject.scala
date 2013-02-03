/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.protocol

import ayla.client._
import ayla.server._
import ayla.client.ui.ConfirmationDialog
import java.io._
import ayla.linalg.PointCloudData
import ayla.pdb.ZipFilePdbStreamProvider
import org.apache.commons.compress.archivers.zip.ZipFile
import scala.io.Source
import ayla.pdb.FilePdbStreamProvider
import ayla.dataset.PdbProviderFiles
import scalaz.{ Success, Failure }
import ayla.client.ui.MessageDialog
import ayla.dataset.CachedDataset
import ayla.preprocess.DomainApproximator
import ayla.geometry.ScalarFunction
import ayla.geometry.ct.ContourTree

case class ConnectToProjectRequest(datasetName: String, projName: String, sfName: String, userName: String) extends MsgFromClient {
  def serverDo(server: AylaServer, oosServer: ObjectOutputStream) = {
    val datasetDir = new File(server.datasetsRootDir, datasetName)
    
    val dataset = server.userSessions.find(session => session.projInfo.datasetName == datasetName).map(session => Success(session.dataset)).getOrElse {
      println("Loading dataset " + datasetName)
      // Load the dataset
      val origPoints = PointCloudData.load(new File(datasetDir, "pcd_v3.dat"))
      val scalarArrayFiles = new File(datasetDir, "scalar_functions").listFiles
      val dsspOutput: Option[Array[Char]] = {
        val dsspFile = new File(datasetDir, "secondary_structure_dssp.dat")
        if (!dsspFile.exists()) {
          println("No DSSP file found.")
          None
        } else {
          println("Found DSSP file.")
          val ois = new ObjectInputStream(new FileInputStream(dsspFile))
          Some(ois.readObject.asInstanceOf[Array[Char]])
        }
      }

      for (pdbProvider <- PdbProviderFiles.validate(datasetDir)) yield new CachedDataset(origPoints, scalarArrayFiles, dsspOutput, pdbProvider.create)
    }
    
    dataset.fail.map { msg =>
      MessageDialog.showMessage(s"The dataset $datasetName has a misconfigured PDB file provider, and Ayla will now exit.\nPlease contact the developers.", "Ayla Configuration Error")
      System.exit(0)
    }

    dataset.map { dataset =>
      val collabProj = server.userSessions.find{session =>
        session.projInfo.datasetName == datasetName &&
        session.projInfo.projName == projName &&
        session.projInfo.sfName == sfName
      }.map(_.collabProject).getOrElse {
      	val (sc, sampledToUnsampled) = DomainApproximator.loadDomain(new File(datasetDir, "collab_projects/" + projName), dataset)
      	val source = Source.fromFile(new File(datasetDir, "scalar_functions/" + sfName))
	      val allFuncVals = source.getLines.map(_.toFloat).toArray
	      source.close
	      CollaborationProject(projName, new ScalarFunction(sc, sampledToUnsampled.map(allFuncVals)), sampledToUnsampled)
      }
      
      server.userSessions += UserSession(userName, ProjInfo(datasetName, projName, sfName), dataset, collabProj)
      
      replyWith(oosServer) {
        ConnectToProjectResponse(collabProj)
      }
    }
  }
}

case class ConnectToProjectResponse(proj: CollaborationProject) extends MsgFromServer {
  def clientDo(client: AylaClient, oosClient: ObjectOutputStream) = replyWith(oosClient) {
    client.ct = ContourTree(proj.sf).simplify(65)
    client.sf = proj.sf
    println("Preparing to calculate contour tree edge areas")
    val edges = client.ct.criticalNodeToIncidentEdges.values.flatten.toArray.distinct
    val vertBatches = edges.map(e => Array(e.n1.vertex, e.n2.vertex) ++ e.noncriticalNodes.map(_.vertex))
    GetContourTreeAreasRequest(client.userName, vertBatches)
//    replyWith(oosClient) {
//      GetContourTreeAreasRequest(client.userName, client.ct)
//    }
  }
}
