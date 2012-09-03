package edu.osu.compgeom.ayla.collab
import javax.media.j3d.Transform3D
import java.util.Date
import java.awt.datatransfer.{Transferable, DataFlavor}
import java.awt.datatransfer.UnsupportedFlavorException

@SerialVersionUID(1L)
trait AylaAnnotation extends Serializable {
	val terrainCameraTransform: Array[Double]
	val name: String
	val timestamp = new Date
	var text: String = ""
	var visible = true
	override def toString = "%s [%s]".format(name, timestamp.toString)
}

class ConformationAnnotation(val name: String, val sampledConformationID: Int, val terrainCameraTransform: Array[Double], val pdbLines: Array[String]) extends AylaAnnotation {
	
}

class ConformationAnnotationTransferable(val data: ConformationAnnotation, val move: Boolean = false) extends Transferable {
  val supportedFlavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType)
  def getTransferData(flavor: DataFlavor): Object = {
    if (flavor == supportedFlavor) {
      return (data, move)
    } else if (flavor == DataFlavor.stringFlavor){
      return data.name
    } else {
      throw new UnsupportedFlavorException(flavor)
    }
  }
  
  def isDataFlavorSupported(flavor: DataFlavor): Boolean = {
    flavor == supportedFlavor || flavor == DataFlavor.stringFlavor
  }
  
  def getTransferDataFlavors: Array[DataFlavor] = Array(supportedFlavor, DataFlavor.stringFlavor)
}

//object ConformationAnnotationTransferable {
//  val flavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType)
//}