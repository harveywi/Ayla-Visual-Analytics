/*       __     __ _
*     /\ \ \   / /| |        /\    Ayla Visual Analytics
*    /  \ \ \_/ / | |       /  \   (c) 2011-2012 William Harvey
*   / /\ \ \   /  | |      / /\ \  http://www.cse.ohio-state.edu/~harveywi/ayla
*  / ____ \ | |   | |____ / ____ \ 
* /_/    \_\|_|   |______/_/    \_\
*
*/
package ayla.server

import ayla.dataset.CachedDataset
import ayla.geometry.ScalarFunction

@SerialVersionUID(1L)
case class CollaborationProject(projName: String, sf: ScalarFunction, sampledToUnsampled: Array[Int]) extends Serializable
