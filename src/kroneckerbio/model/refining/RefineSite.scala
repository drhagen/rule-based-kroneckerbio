/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

/*
import kroneckerbio.utilities.NegatableSeq._
import kroneckerbio.model._
import kroneckerbio.model.building._
trait RefineSite{
  def AgentSite: ModelAgentSite
  def State: BuildStateSite
  def Edge: ModelEdgeSite
}
object RefineSite {
  def resolve_partial_build_edges(contact_map: ContactMap, monomer: String, site: String, edges: NegatableSeq[BuildEdge]) = {
    val new_site = contact_map.FindSite(monomer, site)

    val partial_edges = (
      // Keep only the partial edges
      edges.list.collect{
        case edge: BuildPartialEdge => edge
      }
      // Find the ContactAgentSites that correspond to these edges' targets
      .map(contact_map.FindSite(_))
    )

    // Return
    if (edges.literal == true) {
      // Take intersection of valid edges and supplied edges
      new_site.Edges.intersect(partial_edges)
    }
    else {
      // Subtract supplied edges from valid edges
      new_site.Edges.filterNot(valid => partial_edges.contains(valid))
    }
  }

  def resolve_unbound_build_edges(contact_map: ContactMap, monomer: String, site: String, edges: NegatableSeq[BuildEdge]) = {
    // Return an unbound if an unbound is valid and (it appears in list while literal is true or it does not appear
    // while literal is false)
    val agent_site = contact_map.FindSite(monomer, site)

    if (
      agent_site.IsUnbound
      && (
        !edges.list.find(_.isInstanceOf[BuildUnboundEdge]).isEmpty && edges.literal
        ||
        edges.list.find(_.isInstanceOf[BuildUnboundEdge]).isEmpty && !edges.literal
      )
    ) {
      true
    }
    else {
      false
    }
  }
}

trait RefineReactantSite extends RefineSite {
  def Edge: ModelReactantEdgeSite

  def CanReachRemoteContract(disjoint: ModelReactantGraph$) = Edge.CanReachRemoteContract(disjoint)
  def ReactantNeedsRemoteRefinementForContract(disjoint: ModelReactantGraph$) = Edge.ReactantNeedsRemoteRefinementForContract(disjoint)
  def RefineSiteEdgeToward(distal_site: ModelAgentSite) = Edge.RefineToward(distal_site)
  def RefineEdgeSiteAgainstLocal(distal_site: ModelAgentSite) = Edge.RefineAgainst(distal_site)
}
trait RefineProductSite extends RefineSite

case class RefineConsumedSite(var AgentSite: ModelAgentSite, var State: BuildConsumedStateSite, var Edge: ModelConsumedEdgeSite) extends RefineReactantSite {
  def copy(
    AgentSite: ModelAgentSite = this.AgentSite,
    State: BuildConsumedStateSite = this.State.copy(),
    Edge: ModelConsumedEdgeSite = this.Edge.copy()
  ) = new RefineConsumedSite(AgentSite, State, Edge)
}

object RefineConsumedSite {
  def apply(contact_map: ContactMap, monomer: BuildConsumedMonomer, original: BuildConsumedSite): RefineConsumedSite = {
    // Get agent site corresponding to this site
    val agent_site = contact_map.FindSite(monomer.Name, original.Name)

    // Resolve edges
    val edge = ModelConsumedEdgeSite(contact_map, original.Edge, agent_site)

    // Build site
    new RefineConsumedSite(agent_site, original.State, edge)
  }
}

case class RefineProducedSite(var AgentSite: ModelAgentSite, var State: BuildProducedStateSite, var Edge: ModelProducedEdgeSite) extends RefineProductSite {
  def copy(
    AgentSite: ModelAgentSite = this.AgentSite,
    State: BuildProducedStateSite = this.State.copy(),
    Edge: ModelProducedEdgeSite = this.Edge.copy()
  ) = new RefineProducedSite(AgentSite, State, Edge)
}

object RefineProducedSite {
  def apply(contact_map: ContactMap, monomer: BuildProducedMonomer, original: BuildProducedSite): RefineProducedSite = {
    // Get agent site corresponding to this site
    val agent_site = contact_map.FindSite(monomer.Name, original.Name)

    // Resolve edge
    val edge = ModelProducedEdgeSite(contact_map, original.Edge, agent_site)

    // Return
    new RefineProducedSite(agent_site, original.State, edge)
  }
}

case class RefineConservedSite(var AgentSite: ModelAgentSite, var State: BuildConservedStateSite, var Edge: ModelConservedEdgeSite) extends RefineReactantSite with RefineProductSite {
  def copy(
    AgentSite: ModelAgentSite = this.AgentSite,
    State: BuildConservedStateSite = this.State.copy(),
    Edge: ModelConservedEdgeSite = this.Edge.copy()
  ) = new RefineConservedSite(AgentSite, State, Edge)
}

object RefineConservedSite {
  def apply(contact_map: ContactMap, monomer: BuildConservedMonomer, original: BuildConservedSite): RefineConservedSite = {
    // Get site corresponding to this site
    val agent_site = contact_map.FindSite(monomer.Name, original.Name)

    // Resolve edge
    val edge = ModelConservedEdgeSite(contact_map, original.Edge, agent_site)

    // Attach site edge to site
    new RefineConservedSite(agent_site, original.State, edge)
  }
}

case class RefineObservedSite(var AgentSite: ModelAgentSite, var State: BuildStaticStateSite, var Edge: ModelStaticEdgeSite) extends RefineReactantSite with RefineProductSite {
  def copy(
    AgentSite: ModelAgentSite = this.AgentSite,
    State: BuildStaticStateSite = this.State.copy(),
    Edge: ModelStaticEdgeSite = this.Edge.copy()
  ) = new RefineObservedSite(AgentSite, State, Edge)
}

object RefineObservedSite {
  def apply(contact_map: ContactMap, monomer: BuildStaticMonomer, original: BuildObservedSite): RefineObservedSite = {
    // Get agent site corresponding to this site
    val agent_site = contact_map.FindSite(monomer.Name, original.Name)

    // Resolve edge
    val edge = ModelStaticEdgeSite(contact_map, original.Edge, agent_site)

    // Build site
    RefineObservedSite(agent_site, original.State, edge)

  }

  def apply(
    AgentSite: ModelAgentSite,
    State: Option[BuildStaticStateSite] = None,
    Edge: Option[ModelStaticEdgeSite] = None
  ): RefineObservedSite = new RefineObservedSite(
    AgentSite,
    State.getOrElse(BuildStaticStateSite(AgentSite.States)),
    Edge.getOrElse(ModelStaticLocalEdgeSite(AgentSite.Edges map {ModelPartialEdge(_)}))
  )
}
*/
