/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

import kroneckerbio.model._

trait ModelLocalEdge

case object ModelUnboundEdge extends ModelLocalEdge {
  override def toString = "0"
}

case class ModelPartialEdge(Site: ModelAgentSite) extends ModelLocalEdge {
  override def toString = Site.Parent.Name + "@" + Site.Name
}
