/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.util

import java.io._
import scalaz.Validation._

package object tools {
  def fileOpt(name: String)(implicit dir: File) = {
    val f = new File(dir, name)
    if (f.exists) Some(f) else None
  }
  
  def catchException[T](op: => T, errorMsg: String = "An error occurred") = fromTryCatch(op).fail.map(e => s"$errorMsg:  ${e.getMessage}").validation
}
