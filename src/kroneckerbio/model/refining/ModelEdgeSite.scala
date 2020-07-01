/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

import kroneckerbio.model._
import kroneckerbio.model.refining.refining._
import kroneckerbio.utilities.EqualsByReference
import scala.collection.mutable.Buffer
import kroneckerbio.utilities.RichSeq._
import kroneckerbio.utilities.RichAny._

sealed trait ModelEdgeSite extends EqualsByReference {
  type ThisType <: ModelEdgeSite

  def AgentSite: ModelAgentSite

  def This: ThisType

  def Copy(): ThisType

  def Reduced(edge_site_map: Buffer[(ModelEdgeSite,ModelEdgeSite)]): ThisType

  def ToStringWithEdgeNumbers(edge_number_map: Map[ModelEdgeSite,Int]): String
  def BoundEdgeConnectsHere(edge: ModelBoundEdge): Boolean = edge.Targets.contains(this)
}

object ModelEdgeSite {
  def convert_any_edges(agent_site: ModelAgentSite) =
    agent_site.Edges.map(ModelPartialEdge(_)) :+ ModelUnboundEdge

  def convert_true_local_edges(originals: Seq[LocalEdge], agent_site: ModelAgentSite, agents: Seq[ModelAgent]) = {
    // Convert agent sites to partial edges
    val possible_edges = agent_site.Edges.map(ModelPartialEdge(_))

    val partial_edges = (
      possible_edges
      .intersect(
        originals.collect{
          case PartialEdge(monomer,site) =>
            agents
            .FindOrDie(_.Name == monomer)
            .Sites.FindOrDie(_.Name == site)
            .pipe(ModelPartialEdge(_))
        }
      )
    )

    val option_unbound = if (originals.contains(UnboundEdge)) Some(ModelUnboundEdge) else None

    // Return
    partial_edges ++ option_unbound
    //partial_edges :+ ModelUnboundEdge
  }

  def convert_negated_local_edges(originals: Seq[LocalEdge], agent_site: ModelAgentSite, agents: Seq[ModelAgent]) = {
    // Convert agent sites to partial edges
    val possible_edges = agent_site.Edges.map(ModelPartialEdge(_))

    val partial_edges = (
      possible_edges
      .diff(
        originals.collect{
          case PartialEdge(monomer,site) =>
            agents
            .FindOrDie(_.Name == monomer)
            .Sites.FindOrDie(_.Name == site)
            .pipe(ModelPartialEdge(_))
        }
      )
    )

    val option_unbound = if (originals.contains(UnboundEdge)) None else Some(ModelUnboundEdge)

    // Return
    partial_edges ++ option_unbound
    //partial_edges :+ ModelUnboundEdge
  }
}


///////////////////
//// Factories ////
///////////////////
object ModelConsumedEdgeSite {
  import ModelEdgeSite._

  def apply(
    original: EdgeSite,
    site_name: String,
    agent: ModelAgent,
    monomer_index: Int,
    site_index: Int,
    agents: Seq[ModelAgent],
    edge_map: Map[Int,Buffer[(ModelEdgeSite,Int,Int)]]
  ): ModelConsumedEdgeSite = {
    val agent_site = agent.Sites.FindOrDie(_.Name == site_name)

    original match {
      case AnySiteEdge() =>
        ModelConsumedLocalEdgeSite(agent_site, convert_any_edges(agent_site))

      case BoundSiteEdge(edge) =>  {
        // Create a null bound site, add it to the map, then return the site
        val site = ModelConsumedBoundEdgeSite(agent_site, null)
        edge_map(edge.Index) += Tuple3(site, monomer_index, site_index)
        site
      }

      case TrueLocalSiteEdge(edges) =>
        ModelConsumedLocalEdgeSite(agent_site, convert_true_local_edges(edges, agent_site, agents))

      case NegatedLocalSiteEdge(edges) =>
        ModelConsumedLocalEdgeSite(agent_site, convert_negated_local_edges(edges, agent_site, agents))
    }
  }

  def apply(agent_site: ModelAgentSite): ModelConsumedEdgeSite = {
    ModelConsumedLocalEdgeSite(agent_site, agent_site.Edges.map(ModelPartialEdge(_)) :+ ModelUnboundEdge)
  }
}

object ModelProducedEdgeSite {
  import ModelEdgeSite._

  def apply(
    original: EdgeSite,
    site_name: String,
    agent: ModelAgent,
    monomer_index: Int,
    site_index: Int,
    agents: Seq[ModelAgent],
    edge_map: Map[Int,Buffer[(ModelEdgeSite,Int,Int)]]
  ): ModelProducedEdgeSite = {
    val agent_site = agent.Sites.FindOrDie(_.Name == site_name)

    original match {
      // Produced edges cannot be ambiguous
      //case AnySiteEdge() =>

      case BoundSiteEdge(edge) =>  {
        // Create a null bound site, add it to the map, then return the site
        val site = ModelProducedBoundEdgeSite(agent_site, null)
        edge_map(edge.Index) += Tuple3(site, monomer_index, site_index)
        site
      }

      case TrueLocalSiteEdge(Seq(UnboundEdge)) =>
        ModelProducedUnboundEdgeSite(agent_site)

      // Produced edges cannot be ambiguous
      //case NegatedLocalSiteEdge(edges) =>
    }
  }
}

object ModelConservedEdgeSite {
  import ModelEdgeSite._

  def apply(
    original: EdgeSite,
    site_name: String,
    agent: ModelAgent,
    monomer_index: Int,
    site_index: Int,
    agents: Seq[ModelAgent],
    edge_map: Map[Int,Buffer[(ModelEdgeSite,Int,Int)]]
  ): ModelStaticEdgeSite = {
    val agent_site = agent.Sites.FindOrDie(_.Name == site_name)

    original match {
      case AnySiteEdge() =>
        ModelStaticLocalEdgeSite(agent_site, convert_any_edges(agent_site))

      case BoundSiteEdge(edge) =>  {
        // Create a null bound site, add it to the map, then return the site
        val site = ModelStaticBoundEdgeSite(agent_site, null)
        edge_map(edge.Index) += Tuple3(site, monomer_index, site_index)
        site
      }

      case TrueLocalSiteEdge(edges) =>
        ModelStaticLocalEdgeSite(agent_site, convert_true_local_edges(edges, agent_site, agents))

      case NegatedLocalSiteEdge(edges) =>
        ModelStaticLocalEdgeSite(agent_site, convert_negated_local_edges(edges, agent_site, agents))
    }
  }
}

object ModelStaticEdgeSite {
  import ModelEdgeSite._

  def apply(
    original: EdgeSite,
    site_name: String,
    agent: ModelAgent,
    agents: Seq[ModelAgent],
    edge_map: Map[Int,Buffer[ModelEdgeSite]]
  ): ModelStaticEdgeSite = {
    val agent_site = agent.Sites.FindOrDie(_.Name == site_name)

    original match {
      case AnySiteEdge() =>
        ModelStaticLocalEdgeSite(agent_site, convert_any_edges(agent_site))

      case BoundSiteEdge(edge) =>  {
        // Create a null bound site, add it to the map, then return the site
        val site = ModelStaticBoundEdgeSite(agent_site, null)
        edge_map(edge.Index) += site
        site
      }

      case TrueLocalSiteEdge(edges) =>
        ModelStaticLocalEdgeSite(agent_site, convert_true_local_edges(edges, agent_site, agents))

      case NegatedLocalSiteEdge(edges) =>
        ModelStaticLocalEdgeSite(agent_site, convert_negated_local_edges(edges, agent_site, agents))
    }
  }

  def apply(agent_site: ModelAgentSite): ModelStaticEdgeSite = {
    ModelStaticLocalEdgeSite(agent_site, agent_site.Edges.map(ModelPartialEdge(_)) :+ ModelUnboundEdge)
  }
}


////////////////
//// Traits ////
////////////////

// Location
sealed trait ModelReactantEdgeSite extends ModelEdgeSite {
  type ThisType <: ModelReactantEdgeSite

  def Observed: Seq[ModelLocalEdge]

  def ToStaticReactant: ModelStaticEdgeSite

  def ObeysContract(contract: ModelStaticEdgeSite): Boolean
  def DisobeysContract(contract: ModelStaticEdgeSite): Boolean
}

sealed trait ModelProductEdgeSite extends ModelEdgeSite {
  type ThisType <: ModelProductEdgeSite

  def Resulting: Seq[ModelLocalEdge]

  def ToStaticProduct: ModelStaticEdgeSite
}

sealed trait ModelConservedEdgeSite extends ModelReactantEdgeSite with ModelProductEdgeSite {
  type ThisType <: ModelConservedEdgeSite
}

// Starting structure
sealed trait ModelObservingLocalEdgeSite extends ModelReactantEdgeSite {
  type ThisType <: ModelObservingLocalEdgeSite

  def Copy(Observed: Seq[ModelLocalEdge]): ThisType

  def Reduced(edge_site_map: Buffer[(ModelEdgeSite,ModelEdgeSite)]) = {
    // Create the replacement site
    val new_edge_site =
      Copy(Observed = Observed.intersect(AgentSite.Edges.map(ModelPartialEdge(_)) :+ ModelUnboundEdge))

    // Update map
    edge_site_map += Tuple2(this, new_edge_site)

    // Return
    new_edge_site
  }

  def AllPartialEdges: Seq[ModelPartialEdge] = Observed.collect{case x: ModelPartialEdge => x}

  def CanReachRemoteContract(contract: ModelStaticGraph) = {
    //#TODO: Overcome this limitation to allow for more complex disjoint
    assert(contract.Monomers.size == 1)

    AllPartialEdges.flatMap(
      // Get all reachable monomers
      _.Site.EntryReachableMonomers
      // See if any monomer could meet the contract
    ).exists(
      agent =>
        !ModelStaticMonomer(agent).DisobeysContract(contract.Monomers(0))
    )
  }

  def ReactantNeedsRemoteRefinementForContract(disjoint: ModelStaticGraph): Option[ModelAgentSite] = {
    AllPartialEdges.map(_.Site).find(
      // that has a reachable monomer
      _.EntryReachableMonomers.exists(
        // that can match the contract
        !ModelStaticMonomer(_).DisobeysContract(disjoint.Monomers(0))
      )
    )
  }

  def AllowsNewBindingTo(agent_site: ModelAgentSite) = {
    Observed.collect{case ModelPartialEdge(contract_site) => contract_site}.contains(agent_site)
  }

  def ObeysContract(contract: ModelStaticEdgeSite): Boolean = {
    contract match {
      case ModelStaticLocalEdgeSite(agent_site, observed) =>
        Observed.diff(observed).isEmpty
      case ModelStaticBoundEdgeSite(agent_site, static) =>
        false
    }
  }

  def DisobeysContract(contract: ModelStaticEdgeSite): Boolean =
    Observed.intersect(contract.Observed).isEmpty

  def RefineReactantTowardLocal(contract: ModelObservingLocalEdgeSite): ThisType = {
    Copy(Observed = Observed.intersect(contract.Observed))
  }

  def RefineReactantTowardBound(to_site: ModelAgentSite): ModelObservingBoundEdgeSite

  def RefineReactantAgainstLocal(contract: ModelObservingLocalEdgeSite): ThisType = {
    Copy(Observed = Observed.diff(contract.Observed))
  }

  def RefineReactantAgainstBound(to_site: ModelAgentSite): ThisType = {
    Copy(Observed = Observed.diff(Seq(ModelPartialEdge(to_site))))
  }
}

sealed trait ModelObservingBoundEdgeSite extends ModelReactantEdgeSite {
  type ThisType <: ModelObservingBoundEdgeSite

  def Observed: Seq[ModelPartialEdge]

  def Reduced(edge_site_map: Buffer[(ModelEdgeSite,ModelEdgeSite)]) = This

  def ObeysContract(contract: ModelStaticEdgeSite): Boolean =
    Observed.diff(contract.Observed).isEmpty

  def DisobeysContract(contract: ModelStaticEdgeSite): Boolean =
    Observed.intersect(contract.Observed).isEmpty
}

// Final structure
sealed trait ModelResultingLocalEdgeSite extends ModelProductEdgeSite {
  type ThisType <: ModelResultingLocalEdgeSite
}

sealed trait ModelResultingBoundEdgeSite extends ModelProductEdgeSite {
  type ThisType <: ModelResultingBoundEdgeSite
}

// Changing structure
sealed trait ModelConsumingEdgeSite extends ModelReactantEdgeSite

trait ModelProducingEdgeSite extends ModelProductEdgeSite {
  def Produced: ModelLocalEdge
}

sealed trait ModelConsumingLocalEdgeSite extends ModelConsumingEdgeSite with ModelObservingLocalEdgeSite {
  type ThisType <: ModelConsumingLocalEdgeSite

  def RefineReactantTowardBound(to_site: ModelAgentSite): ModelConsumingBoundEdgeSite
}

sealed trait ModelConsumingBoundEdgeSite extends ModelConsumingEdgeSite with ModelObservingBoundEdgeSite {
  type ThisType <: ModelConsumingBoundEdgeSite
}

sealed trait ModelProducingUnboundEdgeSite extends ModelProducingEdgeSite with ModelResultingLocalEdgeSite {
  type ThisType <: ModelProducingUnboundEdgeSite
}

sealed trait ModelProducingBoundEdgeSite extends ModelProducingEdgeSite with ModelResultingBoundEdgeSite {
  type ThisType <: ModelProducingBoundEdgeSite
}

// Type
sealed trait ModelConsumedEdgeSite extends ModelReactantEdgeSite with ModelConsumingEdgeSite {
  type ThisType <: ModelConsumedEdgeSite
}

sealed trait ModelProducedEdgeSite extends ModelProductEdgeSite with ModelProducingEdgeSite {
  type ThisType <: ModelProducedEdgeSite

  def Reduced(edge_site_map: Buffer[(ModelEdgeSite,ModelEdgeSite)]) = This
}

sealed trait ModelChangedEdgeSite extends ModelConservedEdgeSite with ModelConsumingEdgeSite with ModelProducingEdgeSite {
  type ThisType <: ModelChangedEdgeSite
}

sealed trait ModelStaticEdgeSite extends ModelConservedEdgeSite {
  type ThisType <: ModelStaticEdgeSite

  def IsConsumedBy(reactant: ModelReactantEdgeSite): Boolean
  def IsProducedBy(reactant: ModelProductEdgeSite): Boolean
}


/////////////////
//// Classes ////
/////////////////

// Consumed
final case class ModelConsumedLocalEdgeSite(
  AgentSite: ModelAgentSite,
  Consumed: Seq[ModelLocalEdge]
) extends ModelConsumedEdgeSite with ModelConsumingLocalEdgeSite {
  type ThisType = ModelConsumedLocalEdgeSite

  def Observed: Seq[ModelLocalEdge] = Consumed

  def ToStaticReactant = ModelStaticLocalEdgeSite(AgentSite, Consumed)

  def This = this
  def Copy() = copy()
  def Copy(Observed: Seq[ModelLocalEdge]) = copy(Consumed = Observed)

  def RefineReactantTowardBound(to_site: ModelAgentSite) = ModelConsumedBoundEdgeSite(AgentSite, ModelPartialEdge(to_site))

  def ToStringWithEdgeNumbers(edge_number_map: Map[ModelEdgeSite,Int]) =
    AgentSite.Name + "-" + Consumed.mkString("{",",","}")
}

final case class ModelConsumedBoundEdgeSite(
  AgentSite: ModelAgentSite,
  Consumed: ModelPartialEdge
) extends ModelConsumedEdgeSite with ModelConsumingBoundEdgeSite {
  type ThisType = ModelConsumedBoundEdgeSite

  def Observed: Seq[ModelPartialEdge] = Seq(Consumed)

  def This = this
  def Copy() = copy()

  def ToStaticReactant = ModelStaticBoundEdgeSite(AgentSite, Consumed)

  def ToStringWithEdgeNumbers(edge_number_map: Map[ModelEdgeSite,Int]) =
    AgentSite.Name + "-" + edge_number_map(this)
}


// Produced
final case class ModelProducedUnboundEdgeSite(
  AgentSite: ModelAgentSite
) extends ModelProducedEdgeSite with ModelProducingUnboundEdgeSite {
  type ThisType = ModelProducedUnboundEdgeSite

  def Produced = ModelUnboundEdge
  def Resulting: Seq[ModelLocalEdge] = Seq(ModelUnboundEdge)

  def This = this
  def Copy() = copy()

  def ToStaticProduct = ModelStaticLocalEdgeSite(AgentSite, Seq(ModelUnboundEdge))

  def ToStringWithEdgeNumbers(edge_number_map: Map[ModelEdgeSite,Int]) =
    AgentSite.Name
}

final case class ModelProducedBoundEdgeSite(
  AgentSite: ModelAgentSite,
  Produced: ModelPartialEdge
) extends ModelProducedEdgeSite with ModelProducingBoundEdgeSite {
  type ThisType = ModelProducedBoundEdgeSite

  def Resulting: Seq[ModelPartialEdge] = Seq(Produced)

  def This = this
  def Copy() = copy()

  def ToStaticProduct = ModelStaticBoundEdgeSite(AgentSite, Produced)

  def ToStringWithEdgeNumbers(edge_number_map: Map[ModelEdgeSite,Int]) =
    AgentSite.Name + "-" + edge_number_map(this)
}

// Changed
final case class ModelChangedLocalUnboundEdgeSite(
  AgentSite: ModelAgentSite,
  Consumed: Seq[ModelLocalEdge]
) extends ModelChangedEdgeSite with ModelConsumingLocalEdgeSite with ModelProducingUnboundEdgeSite {
  type ThisType = ModelChangedLocalUnboundEdgeSite

  def Observed: Seq[ModelLocalEdge] = Consumed
  def Produced = ModelUnboundEdge
  def Resulting: Seq[ModelLocalEdge] = Seq(ModelUnboundEdge)

  def This = this
  def Copy() = copy()
  def Copy(Observed: Seq[ModelLocalEdge]) = copy(Consumed = Observed)

  def ToStaticReactant = ModelStaticLocalEdgeSite(AgentSite, Consumed)
  def ToStaticProduct = ModelStaticLocalEdgeSite(AgentSite, Seq(ModelUnboundEdge))

  def RefineReactantTowardBound(to_site: ModelAgentSite) = ModelChangedBoundUnboundEdgeSite(AgentSite, ModelPartialEdge(to_site))

  override def toString = AgentSite.Name + Consumed.mkString("{",",","}") + "->0"

  def ToStringWithEdgeNumbers(edge_number_map: Map[ModelEdgeSite,Int]) =
    AgentSite.Name + "-" + Consumed.mkString("{",",","}")
}

final case class ModelChangedBoundUnboundEdgeSite(
  AgentSite: ModelAgentSite,
  Consumed: ModelPartialEdge
) extends ModelChangedEdgeSite with ModelConsumingBoundEdgeSite with ModelProducingUnboundEdgeSite {
  type ThisType = ModelChangedBoundUnboundEdgeSite

  def Observed: Seq[ModelPartialEdge] = Seq(Consumed)
  def Produced = ModelUnboundEdge
  def Resulting: Seq[ModelLocalEdge] = Seq(ModelUnboundEdge)

  def This = this
  def Copy() = copy()

  def ToStaticReactant = ModelStaticBoundEdgeSite(AgentSite, Consumed)
  def ToStaticProduct = ModelStaticLocalEdgeSite(AgentSite, Seq(ModelUnboundEdge))

  override def toString = AgentSite.Name + "-#->0"

  def ToStringWithEdgeNumbers(edge_number_map: Map[ModelEdgeSite,Int]) =
    AgentSite.Name + "-" + edge_number_map(this)
}

final case class ModelChangedLocalBoundEdgeSite(
  AgentSite: ModelAgentSite,
  Consumed: Seq[ModelLocalEdge],
  Produced: ModelPartialEdge
) extends ModelChangedEdgeSite with ModelConsumingLocalEdgeSite with ModelProducingBoundEdgeSite {
  type ThisType = ModelChangedLocalBoundEdgeSite

  def Observed: Seq[ModelLocalEdge] = Consumed
  def Resulting: Seq[ModelPartialEdge] = Seq(Produced)

  def This = this
  def Copy() = copy()
  def Copy(Observed: Seq[ModelLocalEdge]) = copy(Consumed = Observed)

  def ToStaticReactant = ModelStaticLocalEdgeSite(AgentSite, Consumed)
  def ToStaticProduct = ModelStaticBoundEdgeSite(AgentSite, Produced)

  def RefineReactantTowardBound(to_site: ModelAgentSite) = ModelChangedBoundBoundEdgeSite(AgentSite, ModelPartialEdge(to_site), Produced)

  override def toString = AgentSite.Name + "-" + Consumed.mkString("{",",","}") + "->#"

  def ToStringWithEdgeNumbers(edge_number_map: Map[ModelEdgeSite,Int]) =
    AgentSite.Name + "-" + Consumed.mkString("{",",","}")
}

final case class ModelChangedBoundBoundEdgeSite(
  AgentSite: ModelAgentSite,
  Consumed: ModelPartialEdge,
  Produced: ModelPartialEdge
) extends ModelChangedEdgeSite with ModelConsumingBoundEdgeSite with ModelProducingBoundEdgeSite {
  type ThisType = ModelChangedBoundBoundEdgeSite

  def Observed: Seq[ModelPartialEdge] = Seq(Consumed)
  def Resulting: Seq[ModelPartialEdge] = Seq(Produced)

  def This = this
  def Copy() = copy()

  def ToStaticReactant = ModelStaticBoundEdgeSite(AgentSite, Consumed)
  def ToStaticProduct = ModelStaticBoundEdgeSite(AgentSite, Produced)

  override def toString = AgentSite.Name + "-#->#"

  def ToStringWithEdgeNumbers(edge_number_map: Map[ModelEdgeSite,Int]) =
    AgentSite.Name + "-" + edge_number_map(this)
}


// Static
final case class ModelStaticLocalEdgeSite(
  AgentSite: ModelAgentSite,
  Static: Seq[ModelLocalEdge]
) extends ModelStaticEdgeSite with ModelObservingLocalEdgeSite with ModelResultingLocalEdgeSite {
  type ThisType = ModelStaticLocalEdgeSite

  def Observed: Seq[ModelLocalEdge] = Static
  def Resulting: Seq[ModelLocalEdge] = Static

  def This = this
  def Copy() = copy()
  def Copy(Observed: Seq[ModelLocalEdge]) = copy(Static = Observed)

  def ToStaticReactant = this
  def ToStaticProduct = this

  //#TODO: This should probably only take conserved edge sites
  def IsConsumedBy(reactant_site: ModelReactantEdgeSite): Boolean = (
    reactant_site match {
      case reactant_site: ModelConsumedEdgeSite =>
        !Observed.intersect(reactant_site.Observed).isEmpty

      case reactant_site: ModelChangedEdgeSite =>
        !Observed.intersect(reactant_site.Observed).isEmpty && !Observed.contains(reactant_site.Produced)

      case reactant_site: ModelStaticEdgeSite =>
        false
    }
  )

  //#TODO: This should probably only take conserved edge sites
  def IsProducedBy(product_site: ModelProductEdgeSite): Boolean = {
    product_site match {
      case product_site: ModelProducedEdgeSite =>
        !Observed.intersect(product_site.Resulting).isEmpty

      case product_site: ModelChangedEdgeSite =>
        Observed.contains(product_site.Produced) && !product_site.Observed.diff(Observed).isEmpty

      case product_site: ModelStaticEdgeSite =>
        false
    }
  }

  def RefineReactantTowardBound(to_site: ModelAgentSite) = ModelStaticBoundEdgeSite(AgentSite, ModelPartialEdge(to_site))

  override def toString = AgentSite.Name + "-" + Static.mkString("{",",","}")

  def ToStringWithEdgeNumbers(edge_number_map: Map[ModelEdgeSite,Int]) =
    AgentSite.Name + "-" + Static.mkString("{",",","}")
}

final case class ModelStaticBoundEdgeSite(
  AgentSite: ModelAgentSite,
  Static: ModelPartialEdge
) extends ModelStaticEdgeSite with ModelObservingBoundEdgeSite with ModelResultingBoundEdgeSite {
  type ThisType = ModelStaticBoundEdgeSite

  def Observed: Seq[ModelPartialEdge] = Seq(Static)
  def Resulting: Seq[ModelPartialEdge] = Seq(Static)

  def This = this
  def Copy() = copy()

  def ToStaticReactant = this
  def ToStaticProduct = this

  //#TODO: This should probably only take conserved edge sites
  def IsConsumedBy(reactant_site: ModelReactantEdgeSite): Boolean = (
    reactant_site match {
      case reactant_site: ModelConsumedEdgeSite =>
        reactant_site.Observed.contains(Static)

      case reactant_site: ModelChangedEdgeSite =>
        reactant_site.Observed.contains(Static) && reactant_site.Produced != Static

      case reactant_site: ModelStaticEdgeSite =>
        false
    }
  )

  //#TODO: This should probably only take conserved edge sites
  def IsProducedBy(product_site: ModelProductEdgeSite): Boolean = {
    product_site match {
      case product_site: ModelProducedEdgeSite =>
        product_site.Produced == Static

      case product_site: ModelChangedEdgeSite =>
        product_site.Produced == Static

      case product_site: ModelStaticEdgeSite =>
        false
    }
  }

  override def toString = AgentSite.Name + "-" + Static

  def ToStringWithEdgeNumbers(edge_number_map: Map[ModelEdgeSite,Int]) =
    AgentSite.Name + "-" + edge_number_map(this)
}
