package edu.osu.compgeom.ayla

import scala.swing._
import javax.swing.{ JFrame, JTree }
import javax.swing.tree._
import javax.swing.event.{ TreeSelectionListener, TreeSelectionEvent }
import scala.swing.event.ButtonClicked

@deprecated("Use OpenProjectDialog instead", "0.9")
class OpenAylaProjectDialog(treeRoot: DefaultMutableTreeNode) extends Dialog with TreeSelectionListener {
  var chosenProject: Option[AylaCollaborationProjectDescriptor] = None
  title = "Open Ayla Collaboration Project"
  modal = true
  
  val tree = new JTree(treeRoot)
  tree.addMouseListener(new java.awt.event.MouseAdapter() {
    override def mousePressed(e: java.awt.event.MouseEvent) {
      val selRow = tree.getRowForLocation(e.getX(), e.getY());
      val selPath = tree.getPathForLocation(e.getX(), e.getY());
      if (selRow != -1) {
        if (e.getClickCount() == 2) {
          if (okButton.enabled) {
            close()
          }
        }
      }
    }
  })
  val textArea = new TextArea(0, 80) {
    editable = false
    wordWrap = true
    lineWrap = true
  }

  val okButton = new Button("OK") {
    enabled = false
    reactions += { case e: ButtonClicked => close() }
  }

  val cancelButton = new Button("Quit") {
    chosenProject = None
    reactions += { case e: ButtonClicked => close() }
  }

  contents = new BorderPanel {
    tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION)
    tree.addTreeSelectionListener(OpenAylaProjectDialog.this)

    val treeScrollPane = new ScrollPane(Component.wrap(tree)) {
      minimumSize = new Dimension(100, 50)
    }

    val textScrollPane = new ScrollPane(textArea) {
      minimumSize = new Dimension(100, 50)
    }

    val splitPane = new SplitPane(Orientation.Horizontal) {
      topComponent = treeScrollPane
      bottomComponent = textScrollPane
      dividerLocation = 100
      preferredSize = new Dimension(500, 300)
    }

    val okCancelPanel = new FlowPanel {
      contents += okButton
      contents += cancelButton
    }

    add(splitPane, BorderPanel.Position.Center)
    add(okCancelPanel, BorderPanel.Position.South)
    //contents += splitPane
    //contents += okCancelPanel
  }

  def valueChanged(e: TreeSelectionEvent): Unit = {
    val node = tree.getLastSelectedPathComponent().asInstanceOf[DefaultMutableTreeNode]
    if (node == null) {
      chosenProject = None
      okButton.enabled = false
      displayProjectInfo()
      return
    }

    if (node.isLeaf()) {
      chosenProject = Some(node.getUserObject().asInstanceOf[AylaCollaborationProjectDescriptor])
      okButton.enabled = true
      displayProjectInfo()
    } else {
      chosenProject = None
      okButton.enabled = false
      displayProjectInfo()
    }
  }

  def displayProjectInfo(): Unit = {
    chosenProject match {
      case Some(descriptor) => {
        textArea.text = descriptor.getFormattedDescription
      }
      case None => textArea.text = "--- Welcome to Ayla ---\n\nPlease select a project to open and click OK."
    }
  }

  open()
}

@deprecated("Use OpenProjectDialog instead", "0.9")
object OpenAylaProjectDialog {
  def chooseProject(projectTreeRoot: DefaultMutableTreeNode): Option[AylaCollaborationProjectDescriptor] = {
    val dialog = new OpenAylaProjectDialog(projectTreeRoot)
    dialog.chosenProject
  }
}