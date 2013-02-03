/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/

package ayla.collab

import java.awt.datatransfer.{Transferable, DataFlavor}
import java.awt.datatransfer.UnsupportedFlavorException

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
