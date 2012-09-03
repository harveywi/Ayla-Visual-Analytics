package edu.osu.compgeom.omegavis

import scala.swing._
import scala.swing.event._
import edu.osu.compgeom.omegavis.ColorSchemes.scheme
import javax.swing.BorderFactory
import javax.swing.border.TitledBorder
import javax.swing.UIManager
import edu.osu.compgeom.omegavis.event._

class ConformationSearchResultsFrame(searchResults: Array[(String, Int)], tcManager: TopologicalComponentManager, csPanel: ConformationalSpacePanel) extends Frame {
  title = "Search Results"
    
  preferredSize = new Dimension(500, 500)
    
  listenTo(this)
    
  tcManager.listenTo(this)
  csPanel.pointsPanel.listenTo(this)
  
  class SearchListItem(val conformationName: String, val idx: Int) {
    override def toString = conformationName
  }
  
  reactions += {
    case e: WindowClosing => {
      tcManager.deafTo(this)
      csPanel.pointsPanel.deafTo(this)
    }
  }
    
  val listView = new ListView[SearchListItem] {
    listData = searchResults.map{case (conformationName, idx) => new SearchListItem(conformationName, idx)}
    listenTo(mouse.clicks)
    reactions += {
      case e: MouseClicked => {
        val i = peer.locationToIndex(e.point)
        if (i != -1) {
          val listItem = listData(i)
          
          val allEdges = tcManager.ct.criticalNodeToIncidentEdges.values.flatten.toArray
          val ctEdge = allEdges.find(e =>
            (e.n1.vertex == listItem.idx) || (e.n2.vertex == listItem.idx) || e.noncriticalNodes.iterator.map(_.vertex).contains(listItem.idx)
          ).get
          println("Blah")
          
          // tcManager should listen to me.
          ConformationSearchResultsFrame.this.publish(new SelectionCleared())
          ConformationSearchResultsFrame.this.publish(new EdgeSelected(ctEdge, 1))
          
          // conformationalspacepanel.pointspanel should listen to me.
          ConformationSearchResultsFrame.this.publish(new TopologicalComponentManagerSelectionUpdate(tcManager))
          ConformationSearchResultsFrame.this.publish(new SetS1(listItem.idx))
          // publish TopologicalComponentManagerSelectionUpdate(tcManager)
          // publish SetS1(conformationID)
          
          // set selected conformation here
          
//          storyboardEditor.contents.remove(1, storyboardEditor.contents.size - 1)
//          listItem.annotations.foreach { annotation =>
//            storyboardEditor.contents += new AnnotationArea(annotation)
//            storyboardEditor.contents += new DropArea
//          }
//          storyboardEditor.revalidate()
//          storyboardEditor.repaint()
        }
      }
    }
  }

  val annotationListScrollPane: ScrollPane = new ScrollPane(listView) {
    background = scheme.bgColor
    border = BorderFactory.createTitledBorder(BorderFactory.createLineBorder(scheme.lineColor), "Annotations",
      TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, UIManager.getFont("TitledBorder.font"), scheme.btnForeground)
    minimumSize = new Dimension(100, 50)
  }

  contents = annotationListScrollPane

  pack()
  visible = true
}