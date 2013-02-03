/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.collab

import scala.swing._
import scala.swing.event._
import ayla.client.ui.ColorSchemes.scheme
import javax.swing.BorderFactory
import javax.media.j3d.Transform3D
import javax.swing.border.TitledBorder
import javax.swing.UIManager

class CreateConformationAnnotationDialog(sampledConformationID: Int, pdbLines: Array[String], terrainCameraTransform: Array[Double]) extends Dialog {
  modal = true
  background = scheme.bgColor
  
  var result: Option[ConformationAnnotation] = None

  val nameTextField = new TextField {
    background = scheme.bgColor
    foreground = scheme.btnForeground
  }

  val descriptionTextArea = new TextArea(10, 80) {
    wordWrap = true
    lineWrap = true
    background = scheme.bgColor
    foreground = scheme.btnForeground
  }

  contents = new BorderPanel {
    add(new BorderPanel {
      background = scheme.bgColor
      add(new Label("Name:") {
        background = scheme.btnBackground
        foreground = scheme.btnForeground
      }, BorderPanel.Position.West)
      add(nameTextField, BorderPanel.Position.Center)
    }, BorderPanel.Position.North)
    add(new ScrollPane(descriptionTextArea) {
      background = scheme.bgColor
      border = BorderFactory.createTitledBorder(BorderFactory.createLineBorder(scheme.lineColor), "Description", 
          TitledBorder.DEFAULT_JUSTIFICATION, TitledBorder.DEFAULT_POSITION, UIManager.getFont("TitledBorder.font"), scheme.btnForeground)
      minimumSize = new Dimension(100, 50)
    }, BorderPanel.Position.Center)
    add(new FlowPanel {
      background = scheme.bgColor
      contents += new Button("OK") {
        background = scheme.btnBackground
        foreground = scheme.btnForeground
        reactions += {
          case e: ButtonClicked => {
            
            val annotation = new ConformationAnnotation(nameTextField.text, sampledConformationID, terrainCameraTransform, pdbLines)
            result = Some(annotation)
            close()
          }
        }
      }
      contents += new Button("Cancel") {
        background = scheme.btnBackground
        foreground = scheme.btnForeground
        reactions += {case e: ButtonClicked => {close()}}
      }
    }, BorderPanel.Position.South)
  }
  open()
}

object CreateConformationAnnotationDialog {
  def getAnnotation(sampledConformationID: Int, pdbLines: Array[String], terrainCameraTransform: Array[Double]): Option[ConformationAnnotation] = {
    val dlg = new CreateConformationAnnotationDialog(sampledConformationID, pdbLines, terrainCameraTransform)
    dlg.result
  }
}
