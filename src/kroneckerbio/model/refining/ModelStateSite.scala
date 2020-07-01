/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

import kroneckerbio.model._
import kroneckerbio.utilities.EqualsByReference
import kroneckerbio.utilities.RichSeq._

sealed trait ModelStateSite extends EqualsByReference {
  type ThisType <: ModelStateSite
  def AgentSite: ModelAgentSite

  def Copy(): ThisType

  def Reduced: ThisType
}

object ModelStateSite {
  def resolve_site_states(
    original: StateSite,
    agent_site: ModelAgentSite
  ) = {
    original match {
      case site: AnyStateSite => agent_site.States
      case site: TrueStateSite => agent_site.States.intersect(convert_states(site.states, agent_site.States))
      case site: NegatedStateSite => agent_site.States.diff(convert_states(site.states, agent_site.States))
    }
  }

  def convert_states(original_states: Seq[StateName], agents_states: Seq[ModelAgentState]) =
    agents_states.filter(agent_state => original_states.contains(agent_state.Name))
}

///////////////////
//// Factories ////
///////////////////

object ModelConsumedStateSite {
  import ModelStateSite._

  def apply(
    original: StateSite,
    site_name: String,
    agent: ModelAgent,
    compartments: Seq[ModelCompartment],
    agents: Seq[ModelAgent]
  ): ModelConsumedStateSite = {
    val agent_site = agent.Sites.FindOrDie(_.Name == site_name)

    val states = resolve_site_states(original, agent_site)

    // Return
    ModelConsumedStateSite(agent_site, states)
  }

  def apply(agent_site: ModelAgentSite): ModelConsumedStateSite = {
    ModelConsumedStateSite(agent_site, agent_site.States)
  }
}

object ModelProducedStateSite {
  import ModelStateSite._

  def apply(
    original: StateSite,
    site_name: String,
    agent: ModelAgent,
    compartments: Seq[ModelCompartment],
    agents: Seq[ModelAgent]
  ): ModelProducedStateSite = {
    val agent_site = agent.Sites.FindOrDie(_.Name == site_name)

    val states = resolve_site_states(original, agent_site)

    // Return
    ModelProducedStateSite(agent_site, states(0)) // Must be exactly one
  }
}

object ModelStaticStateSite {
  import ModelStateSite._

  def apply(
    original: StateSite,
    site_name: String,
    agent: ModelAgent,
    compartments: Seq[ModelCompartment],
    agents: Seq[ModelAgent]
  ): ModelStaticStateSite = {
    val agent_site = agent.Sites.FindOrDie(_.Name == site_name)

    val states = resolve_site_states(original, agent_site)

    // Return
    ModelStaticStateSite(agent_site, states)
  }

  def apply(agent_site: ModelAgentSite): ModelStaticStateSite = {
    ModelStaticStateSite(agent_site, agent_site.States)
  }
}

////////////////
//// Traits ////
////////////////

// Location
sealed trait ModelReactantStateSite extends ModelStateSite {
  type ThisType <: ModelReactantStateSite

  def Observed: Seq[ModelAgentState]
  def ToStaticReactant: ModelStaticStateSite

  def Copy(Observed: Seq[ModelAgentState]): ThisType

  def Reduced = Copy(Observed = Observed.intersect(AgentSite.States))

  // Does this match the supplied contract
  def ObeysContract(target: ModelStaticStateSite): Boolean = {
    Observed.forall(target.Observed.contains(_))
  }

  def DisobeysContract(target: ModelStaticStateSite): Boolean = {
    Observed.intersect(target.Observed).isEmpty
  }

  // Refine the compartment
  def RefineReactantToward(refiner: ModelStaticStateSite): ThisType = {
    Copy(Observed = Observed.intersect(refiner.Observed))
  }

  def RefineReactantAgainst(refiner: ModelStaticStateSite): ThisType = {
    Copy(Observed = Observed.diff(refiner.Observed))
  }
}

sealed trait ModelProductStateSite
  extends ModelStateSite {
  type ThisType <: ModelProductStateSite

  def Resulting: Seq[ModelAgentState]

  def ToStaticProduct: ModelStaticStateSite
}

sealed trait ModelConservedStateSite
  extends ModelReactantStateSite
  with ModelProductStateSite {
  type ThisType <: ModelConservedStateSite
}

// Structure
sealed trait ModelConsumingStateSite
  extends ModelReactantStateSite {
  type ThisType <: ModelConsumingStateSite

  def Consumed: Seq[ModelAgentState]
}

sealed trait ModelProducingStateSite
  extends ModelProductStateSite {
  type ThisType <: ModelProducingStateSite

  def Produced: ModelAgentState
}

/////////////////
//// Classes ////
/////////////////

final case class ModelConsumedStateSite(AgentSite: ModelAgentSite, Consumed: Seq[ModelAgentState])
  extends ModelConsumingStateSite {
  type ThisType = ModelConsumedStateSite

  def Observed = Consumed
  def ToStaticReactant = ModelStaticStateSite(AgentSite, Consumed)

  def Copy() = copy()
  def Copy(Observed: Seq[ModelAgentState]) = copy(Consumed = Observed)

  override def toString = AgentSite.Name + "~" + Consumed.map(_.Name).mkString("{",",","}")
}

final case class ModelProducedStateSite(AgentSite: ModelAgentSite, Produced: ModelAgentState)
  extends ModelProducingStateSite {
  type ThisType = ModelProducedStateSite

  def Resulting = Seq(Produced)

  def Reduced = this

  def ToStaticProduct = ModelStaticStateSite(AgentSite, Seq(Produced))

  def Copy() = copy()

  override def toString = AgentSite.Name + "~{" + Produced.Name + "}"
}

final case class ModelChangedStateSite(
  AgentSite: ModelAgentSite,
  Consumed: Seq[ModelAgentState],
  Produced: ModelAgentState
)
  extends ModelConservedStateSite
  with ModelConsumingStateSite
  with ModelProducingStateSite {
  type ThisType = ModelChangedStateSite

  def Observed = Consumed
  def Resulting = Seq(Produced)
  def ToStaticReactant = ModelStaticStateSite(AgentSite, Consumed)
  def ToStaticProduct = ModelStaticStateSite(AgentSite, Seq(Produced))

  def Copy() = copy()
  def Copy(Observed: Seq[ModelAgentState]) = copy(Consumed = Observed)

  override def toString = AgentSite.Name + "~" + (Consumed :+ Produced).map(_.Name).mkString("{",",","}")
}

final case class ModelStaticStateSite(
  AgentSite: ModelAgentSite,
  Static: Seq[ModelAgentState]
)
  extends ModelConservedStateSite {
  type ThisType = ModelStaticStateSite

  def Observed = Static
  def Resulting = Static
  def ToStaticReactant = this
  def ToStaticProduct = this

  def Copy() = copy()

  def IsConsumedBy(reactant_state: ModelReactantStateSite): Boolean = (
    reactant_state match {
      case ModelConsumedStateSite(_, consumed) =>
        !consumed.intersect(Observed).isEmpty

      case ModelChangedStateSite(_, consumed, produced) =>
        !Observed.contains(produced) && !consumed.intersect(Observed).isEmpty

      case _: ModelStaticStateSite =>
        false
    }
  )

  def IsProducedBy(reactant_state: ModelProductStateSite): Boolean = (
    reactant_state match {
      case ModelProducedStateSite(_, produced) =>
        Observed.contains(produced)

      case ModelChangedStateSite(_, consumed, produced) =>
        Observed.contains(produced) && !consumed.diff(Observed).isEmpty

      case _: ModelStaticStateSite =>
        false
    }
  )

  def Copy(Observed: Seq[ModelAgentState]) = copy(Static = Observed)

  override def toString = {
    AgentSite.Name + "~" + Static.map(_.Name).mkString("{",",","}")
  }
}
