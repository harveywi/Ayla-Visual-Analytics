package edu.osu.compgeom.ayla

import scala.swing._
import java.io.{StringWriter, PrintWriter}
import java.awt.Font

class ErrorDialog(exception: Exception) extends Dialog {
  title = "A Problem Occurred"
	modal = true
	
	val errorDetails = {
    val stringWriter = new StringWriter
    exception.printStackTrace(new PrintWriter(stringWriter))
    stringWriter.toString
  }
	
	contents = new BorderPanel {
	  add(new TextArea {
	    font = new Font("Verdana", Font.BOLD, 12)
	    text = "A problem has occurred, and Ayla will now exit.  Please contact the developers.\nError details:"
	  }, BorderPanel.Position.North)
	  
    val errorTextArea = new TextArea(errorDetails, 8, 80) {
      editable = false
    }
    add(new ScrollPane(errorTextArea), BorderPanel.Position.Center)
  }
	
	centerOnScreen()
	open()
}

object ErrorDialog {
  def showErrorAndQuit(e: Exception) = {
    val dlg = new ErrorDialog(e)
    System.exit(0)
  }
}