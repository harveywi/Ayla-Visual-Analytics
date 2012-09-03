package edu.osu.compgeom.ayla

import edu.osu.compgeom.topology.ScalarFunction
import java.io.File
import java.util.Date

@SerialVersionUID(1L)
class AylaCollaborationProject(val file: File, val sf: ScalarFunction, val sampledToUnsampled: Array[Int]) extends Serializable {
  var name: String = ""
  var description: String = ""
  var dateCreated = new Date()
  var dateModified = new Date()
  
  def getDescriptor = new AylaCollaborationProjectDescriptor(file, name, description, dateCreated, dateModified)
  def matchesDescriptor(d: AylaCollaborationProjectDescriptor) = (file == d.file)
}

@SerialVersionUID(1L)
class AylaCollaborationProjectDescriptor(val file: File, val name: String, val description: String, val dateCreated: Date, val dateModified: Date) extends Serializable {
	override def toString = name
	def getFormattedDescription = {
"""|Project Name:
	 |%s
	 |
   |File Name:
	 |%s
	 |
   |Date Created:
	 |%s
	 |
   |Date Modified:
	 |%s
	 |
   |Description:
	 |%s""".stripMargin.format(name, file.getName(), dateCreated.toString(), dateModified.toString(), description)
  }
}
