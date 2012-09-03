package edu.osu.compgeom.ayla.collab

import scala.swing._
import scala.swing.event._
import edu.osu.compgeom.ayla.AylaClient
import edu.osu.compgeom.omegavis.ColorSchemes.scheme
import javax.swing.BorderFactory
import javax.swing.border.TitledBorder
import javax.swing.UIManager
import javax.swing.SwingUtilities
import javax.swing.plaf.basic.BasicListUI
import edu.osu.compgeom.ayla.event._
import javax.swing.{ TransferHandler, JComponent }
import java.awt.datatransfer.StringSelection
import javax.swing.event.ListDataListener
import javax.swing.event.ListDataEvent
import javax.swing.DefaultListModel
import javax.swing.WindowConstants

/**
 * What do we need?
 * 1.  Chat
 * 2.  List of annotations
 * 3.  Annotation storyboards
 */
class AylaCollaborationFrame(client: AylaClient) extends Frame {
  title = "Ayla Collaboration"
  val chatTextArea = new TextArea(10, 80) {
    editable = false
    wordWrap = true
    lineWrap = true
    background = scheme.bgColor
    foreground = scheme.btnForeground
    text = "---Chat Log---\n"
  }
  
  peer.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  val annotationListView: ListView[ConformationAnnotationListItem] = new ListView[ConformationAnnotationListItem] {
    
    override def listData_=(items: Seq[ConformationAnnotationListItem]) = {
      super.listData_=(items)
      println("Putting stuff in list")
      AylaCollaborationFrame.this.contents.foreach(_.revalidate)
    }
    
    var draggedItem: Option[ConformationAnnotation] = None
    peer.setDragEnabled(true)
    peer.setTransferHandler(new TransferHandler {
      override def getSourceActions(c: JComponent) = TransferHandler.COPY
      override def createTransferable(c: JComponent) = {
        val t = selection.items.mkString(",")

        val ret = new ConformationAnnotationTransferable(draggedItem.get)
        draggedItem = None
        ret
      }

    })
    listenTo(mouse.clicks)
    background = scheme.bgColor
    foreground = scheme.btnForeground
    
    renderer = new ListView.Renderer[ConformationAnnotationListItem] {
      def componentFor(list: ListView[_], isSelected: Boolean, focused: Boolean, a: ConformationAnnotationListItem, index: Int) = a
    }

    reactions += {
      case e: MouseClicked => {
        val i = peer.locationToIndex(e.point)
        if (i != -1) {
          val listItem = listData(i)
          //          val ui = peer.getUI().asInstanceOf[BasicListUI]
          //          val cellBounds = ui.getCellBounds(this.peer, i, i)
          //          val listItemClickCoords = new Point(e.point.x, e.point.y % cellBounds.height)
          //          val btnClickCoords = SwingUtilities.convertPoint(listItem.peer, listItemClickCoords, listItem.checkBtn.peer)
          //          if (listItem.checkBtn.peer.contains(btnClickCoords)) {
          listItem.selected = !listItem.selected
          listItem.annotation.visible = listItem.selected
          AylaCollaborationFrame.this.publish(RefreshAnnotationVisibilities)
          repaint()
          //          }
        }
      }
      case e: MousePressed => {
        println("Mouse pressed")
        val i = peer.locationToIndex(e.point)
        if (i != -1) {
          val listItem = listData(i)
          draggedItem = Some(listItem.annotation)
        }
      }
    }
  }

  val annotationListScrollPane: ScrollPane = new ScrollPane(annotationListView) {
    background = scheme.bgColor
    border = BorderFactory.createTitledBorder(BorderFactory.createLineBorder(scheme.lineColor), "Annotations",
      TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, UIManager.getFont("TitledBorder.font"), scheme.btnForeground)
    minimumSize = new Dimension(100, 50)
  }

  val storyboardPanel = new StoryboardPanel(client, this)

  contents = new BorderPanel {
    add(new BorderPanel {
      val textScrollPane = new ScrollPane(chatTextArea) {
        border = BorderFactory.createLineBorder(scheme.lineColor)
        minimumSize = new Dimension(100, 50)
      }
      add(textScrollPane, BorderPanel.Position.Center)

      add(new BorderPanel {
        val textField = new TextField {
          border = BorderFactory.createLineBorder(scheme.lineColor)
          background = scheme.bgColor
          foreground = scheme.btnForeground
          listenTo(keys)
          reactions += {
            case KeyPressed(_, Key.Enter, _, _) => {
              client.postChatMessage(text)
              text = ""
            }
          }
        }
        add(textField, BorderPanel.Position.Center)
        add(new Button("Send") {
          background = scheme.btnBackground
          foreground = scheme.btnForeground
          reactions += {
            case e: ButtonClicked => {
              client.postChatMessage(textField.text)
              textField.text = ""
            }
          }
        }, BorderPanel.Position.East)
      }, BorderPanel.Position.South)

    }, BorderPanel.Position.Center)

    add(annotationListScrollPane, BorderPanel.Position.East)
    add(storyboardPanel, BorderPanel.Position.South)
  }
  pack()
}