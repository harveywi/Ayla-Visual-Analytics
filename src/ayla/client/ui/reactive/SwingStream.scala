package ayla.client.ui.reactive

import scala.swing._
import scala.swing.event._
import reactive._

trait SwingStream {self: Publisher with Reactor =>
	listenTo(self)
	val swingEvents = new EventSource[Event]()
	reactions += {
	  case e: Event => swingEvents.fire(e)
	}
}