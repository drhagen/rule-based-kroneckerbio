/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

import kroneckerbio.utilities.EqualsByReference

sealed trait ModelBoundEdge extends EqualsByReference {
  def Targets: Seq[ModelEdgeSite]

  override def toString = Targets.map(site => site.AgentSite.Parent.Name + "@" + site.AgentSite.Name).mkString("-")
}

sealed trait ModelReactantBoundEdge extends ModelBoundEdge
sealed trait ModelProductBoundEdge extends ModelBoundEdge

case class ModelConsumedBoundEdge(Targets: Seq[ModelEdgeSite]) extends ModelReactantBoundEdge

case class ModelProducedBoundEdge(Targets: Seq[ModelEdgeSite]) extends ModelProductBoundEdge

case class ModelStaticBoundEdge(Targets: Seq[ModelEdgeSite]) extends ModelReactantBoundEdge with ModelProductBoundEdge
