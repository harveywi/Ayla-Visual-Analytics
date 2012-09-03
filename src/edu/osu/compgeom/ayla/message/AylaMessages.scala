package edu.osu.compgeom.ayla.message

import java.io._
import edu.osu.compgeom.ayla.menu._
import edu.osu.compgeom.ayla.AylaCollaborationProject
import java.util.Date
import org.jgrapht.graph.{SimpleDirectedGraph, DefaultEdge}
import edu.osu.compgeom.ayla.collab._
import scala.util.matching.Regex

case class RegisterUserNameRequest(username: String)
case object RegisterUserNameResponse

case object GetCollabProjectListRequest
case class GetCollabProjectListResponse(val menuGraph: PieMenu.PieMenuGraph)

case class GetCollabProjectRequest(val project: (String, File))
case class GetCollabProjectResponse(val proj: AylaCollaborationProject)

case class PostClientOpenedCachedCollabProject(val project: (String, File))
case object PostServerOpenedCachedCollabProject

case class EstimateAreasRequest(val vertBatches: Array[Array[Int]])
case class EstimateAreasResponse(val areas: Array[Double])

case class GetPDBRequest(val pdbID: Int)

/**
 * The compressed bytes contains a gzipped Array[String]. 
 */
case class GetPDBResponse(val compressedBytes: Array[Byte])

case object GetDSSPOutputRequest
case class GetDSSPOutputResponse(val compressedBytesOpt: Option[Array[Byte]])

case object GetColorFunctionsRequest
case class GetColorFunctionsResponse(functions: Array[(File, String)])

case class GetColorFunctionRequest(file: File)
case class GetColorFunctionResponse(function: Array[Float])

case class FindMatchingConformationsRequest(regex: Regex)
case class FindMatchingConformationsResponse(matches: Array[(String, Int)])

case class GetSubsetContactDensitiesRequest(residues: Array[Int])
case class GetSubsetContactDensitiesResponse(contactDensities: Array[Int])

case class GetConformationsAlongShortestPathRequest(idStart: Int, idEnd: Int)
case class GetConformationsAlongShortestPathResponse(val pathConformations: Array[Int])


/*
 * Chat, annotations, storyboards 
 */
case class PostChatMessageToServer(userName: String, text: String)
case class ChatMessageFromServer(whoSentIt: String, text: String, timestamp: Date)

case class PostAnnotationToServer(annotation: ConformationAnnotation)
case object RefreshAnnotationListRequest
case class RefreshAnnotationListResponse(annotations: Array[ConformationAnnotation])

case class PostStoryboardToServer(storyboard: Storyboard)
case object RefreshStoryboardListRequest
case class RefreshStoryboardListResponse(storyboards: Array[Storyboard])