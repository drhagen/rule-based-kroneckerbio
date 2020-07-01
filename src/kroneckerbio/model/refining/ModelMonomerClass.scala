/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

import kroneckerbio.model._
import kroneckerbio.model.refining.refining._
import kroneckerbio.utilities.EqualsByReference
import kroneckerbio.utilities.RichSeq._
import collection.mutable.Buffer

final case class ModelMonomerClass[
  +StateSiteType <: ModelStateSite : Manifest,
  +EdgeSiteType <: ModelEdgeSite : Manifest,
  +CompartmentType <: ModelCompartmentSite : Manifest
](
  Agent: ModelAgent,
  StateSites: Seq[StateSiteType],
  EdgeSites: Seq[EdgeSiteType],
  Compartment: CompartmentType
) extends EqualsByReference {

  type ThisType = ModelMonomerClass[StateSiteType,EdgeSiteType,CompartmentType]

  def manifest_types = (
    implicitly[Manifest[_ <: StateSiteType]],
    implicitly[Manifest[_ <: EdgeSiteType]],
    implicitly[Manifest[_ <: CompartmentType]]
  )

  def DeepCopy() = ModelMonomerClass(
    Agent,
    StateSites.map(_.Copy().asInstanceOf[StateSiteType]),
    EdgeSites.map(_.Copy().asInstanceOf[EdgeSiteType]),
    Compartment.Copy().asInstanceOf[CompartmentType]
  )

  def RebuildEdgeSites[A >: EdgeSiteType, B <: ModelEdgeSite : Manifest](edge_site_map: Map[A,B]): ThisType =
    copy(EdgeSites = EdgeSites.map(edge_site_map(_).asInstanceOf[EdgeSiteType]))

  def Reduced(edge_site_map: Buffer[(ModelEdgeSite,ModelEdgeSite)]): ThisType = {
    val state_sites = Agent.Sites.map{site =>
      val matching_site = StateSites.find(_.AgentSite == site)

      matching_site match {
        case Some(original) => original.Reduced.asInstanceOf[StateSiteType]
        case None if IsModelConsumedMonomer => ModelConsumedStateSite(site).asInstanceOf[StateSiteType]
        //case None if IsModelProducedMonomer => this Produced monomers cannot be ambiguous
        case None if IsModelConservedMonomer => ModelStaticStateSite(site).asInstanceOf[StateSiteType]
        case _ =>
          ModelStaticStateSite(site).asInstanceOf[StateSiteType]
      }
    }

    val edge_sites = Agent.Sites.map{site =>
      val matching_site = EdgeSites.find(_.AgentSite == site)

      matching_site match {
        case Some(original) => original.Reduced(edge_site_map).asInstanceOf[EdgeSiteType]
        case None if IsModelConsumedMonomer => ModelConsumedEdgeSite(site).asInstanceOf[EdgeSiteType]
        //case None if IsModelProducedMonomer => Produced monomers cannot be ambiguous
        case None if IsModelConservedMonomer => ModelStaticEdgeSite(site).asInstanceOf[EdgeSiteType]
        case _ =>
          ModelStaticEdgeSite(site).asInstanceOf[EdgeSiteType]
      }
    }

    val compartment_site = Compartment.Reduced(Agent.Compartments).asInstanceOf[CompartmentType]

    // Return
    copy(StateSites = state_sites, EdgeSites = edge_sites, Compartment = compartment_site)
  }

  def BoundEdgeSitesRebuilt(
    reactant_insertion_map: Map[ModelEdgeSite, ModelPartialEdge],
    produced_insertion_map: Map[ModelEdgeSite, ModelPartialEdge]
  ) = {
    // Rebuild sites
    val edge_sites = {
      for (old_site <- EdgeSites) yield {
        old_site match {
          case old @ ModelConsumedBoundEdgeSite(agent_site, _) =>
            ModelConsumedBoundEdgeSite(agent_site, reactant_insertion_map(old))

          case old @ ModelProducedBoundEdgeSite(agent_site, _) =>
            ModelProducedBoundEdgeSite(agent_site, produced_insertion_map(old))

          case old @ ModelChangedBoundUnboundEdgeSite(agent_site, _) =>
            ModelChangedBoundUnboundEdgeSite(agent_site, reactant_insertion_map(old))

          case old @ ModelChangedLocalBoundEdgeSite(agent_site, consumed, _) =>
            ModelChangedLocalBoundEdgeSite(agent_site, consumed, produced_insertion_map(old))

          case old @ ModelChangedBoundBoundEdgeSite(agent_site, _, _) =>
            ModelChangedBoundBoundEdgeSite(agent_site, reactant_insertion_map(old), produced_insertion_map(old))

          case old @ ModelStaticBoundEdgeSite(agent_site, _) =>
            ModelStaticBoundEdgeSite(agent_site, reactant_insertion_map(old))

          case old => old
        }
      }
    }

    // Return
    copy(EdgeSites = edge_sites.asInstanceOf[Seq[EdgeSiteType]])
  }

  def BoundEdgeConnectsHere(edge: ModelBoundEdge) =
    EdgeSites.exists(_.BoundEdgeConnectsHere(edge))

  def AmbiguousTargetSites(implicit evidence: EdgeSiteType <:< ModelReactantEdgeSite) = {
    // All partial sites that exist on all sites on this monomer
    EdgeSites.collect{case x: ModelObservingLocalEdgeSite => x}.flatMap(_.AllPartialEdges).map(_.Site)
  }

  def EdgeSiteIndex(site: ModelEdgeSite): Option[Int] = (
    EdgeSites.zipWithIndex
    .find(_._1 == site)
    match {
      case Some((_, site_index)) => Some(site_index)
      case None => None
    }
  )

  // Does this match the supplied contract
  def ObeysContract(contract: ModelStaticMonomer)(implicit evidence: ThisType <:< ModelStaticMonomer): Boolean = {
    val new_this = evidence(this)

    val agent_obeys =
      Agent == contract.Agent

    val compartment_obeys =
      new_this.Compartment.ObeysContract(contract.Compartment)

    val edge_sites_obey =
      new_this.EdgeSites.zip(contract.EdgeSites).forall(tuple => tuple._1.ObeysContract(tuple._2))

    val state_sites_obey =
      new_this.StateSites.zip(contract.StateSites).forall(tuple => tuple._1.ObeysContract(tuple._2))

    // Return
    agent_obeys && compartment_obeys && edge_sites_obey && state_sites_obey
  }

  def DisobeysContract(contract: ModelStaticMonomer)(implicit evidence: ThisType <:< ModelStaticMonomer): Boolean = {
    val new_this = evidence(this)

    val agent_disobeys = Agent != contract.Agent

    val compartment_disobeys = new_this.Compartment.DisobeysContract(contract.Compartment)

    val edge_disobeys =
      new_this.EdgeSites.zip(contract.EdgeSites)
      .exists(tuple => tuple._1.DisobeysContract(tuple._2))

    val state_disobeys =
      new_this.StateSites.zip(contract.StateSites)
      .exists(tuple => tuple._1.DisobeysContract(tuple._2))

    // Return
    agent_disobeys || compartment_disobeys || edge_disobeys || state_disobeys
  }

  def ReactantNeedsLocalRefinementForContract(
    contract: ModelStaticMonomer
  )(
    implicit evidence: ThisType <:< ModelReactantMonomer
  ): (
    Option[(CompartmentType, ModelStaticCompartmentSite)],
    Seq[(StateSiteType, ModelStaticStateSite)],
    Seq[(EdgeSiteType with ModelObservingLocalEdgeSite, ModelStaticLocalEdgeSite)]
  ) = {
    if (this.Agent == contract.Agent) {
      // Same monomer types, refinement is possible
      val new_this = evidence(this).asInstanceOf[ModelReactantMonomer with ThisType]

      // Check if compartment needs to be refined
      val option_compartment_pair = {
        if (!(new_this.Compartment.ObeysContract(contract.Compartment) || new_this.Compartment.DisobeysContract(contract.Compartment))) {
          Some((Compartment, contract.Compartment))
        }
        else {
          None
        }
      }

      // Check if state sites need to be refined
      val list_state_sites = {
        new_this.StateSites.zip(contract.StateSites).filter(tuple =>
          (!(tuple._1.ObeysContract(tuple._2) || tuple._1.DisobeysContract(tuple._2)))
        )
      }

      // Check if edge sites need to be refined
      val list_edge_pairs = {
        new_this.EdgeSites.zip(contract.EdgeSites)
        // Only two local edge sites can be refined against each other
        .collect{
          case (
            first: ModelObservingLocalEdgeSite,
            second: ModelStaticLocalEdgeSite
            ) => (first.asInstanceOf[EdgeSiteType with ModelObservingLocalEdgeSite],second)
        }
        .filter(tuple =>
          (!(tuple._1.ObeysContract(tuple._2) || tuple._1.DisobeysContract(tuple._2)))
        )
      }

      // Return all together
      (option_compartment_pair, list_state_sites, list_edge_pairs)

    } else {
      // Different monomers agents, no refinement here
      (None,Nil,Nil)
    }
  }

  // Returns product monomers first, followed by fragment monomers
  def ProductNeedsLocalRefinementForContract(
    contract: ModelStaticMonomer
  )(
    implicit ev: ThisType <:< ModelReactantMonomer
  ): (
    Option[(ModelStaticCompartmentSite, ModelStaticCompartmentSite)],
    Seq[(ModelStaticStateSite, ModelStaticStateSite)],
    Seq[(ModelObservingLocalEdgeSite, ModelStaticLocalEdgeSite)]
  ) = {
    this match {
      case x: ModelConservedMonomer if Agent == contract.Agent && IsModelConservedMonomer =>
        val new_this = x.asInstanceOf[ModelConservedMonomer]

        // Check if compartment needs to be refined
        val option_compartment_pair = {
          Compartment match {
            case compartment: ModelStaticCompartmentSite
              if (!(new_this.Compartment.ToStaticProduct.ObeysContract(contract.Compartment)
                || new_this.Compartment.ToStaticProduct.DisobeysContract(contract.Compartment))) =>
              Some((compartment, contract.Compartment))

            case _ =>
              None
          }
        }

        // Check if state sites need to be refined
        val list_state_sites = {
          new_this.StateSites.zip(contract.StateSites)
          // Only static sites can be refined in this way
          .collect{
            case (first: ModelStaticStateSite, second: ModelStaticStateSite)
              if (!(first.ToStaticProduct.ObeysContract(second)
                || first.ToStaticProduct.DisobeysContract(second))) =>
              (first, second)
          }
        }

         // Check if edge sites need to be refined
        val list_edge_pairs = {
          new_this.EdgeSites.zip(contract.EdgeSites)
          // Only static local edge sites can be refined against each other
          // Evade type erasure by filtering on the interior of the tuple
          // Cast to EdgeSiteType because the collect is type checker is not very smart
          .collect{
            case (first: ModelStaticLocalEdgeSite, second: ModelStaticLocalEdgeSite)
              if (!(first.ToStaticProduct.ObeysContract(second)
                || first.ToStaticProduct.DisobeysContract(second))) =>
              (first, second)
          }
        }

        // Return all together
        (option_compartment_pair, list_state_sites, list_edge_pairs)

      // Either is a consumed monomer or agent doesn't match
      case _ => (None,Nil,Nil)
    }
  }

  def FragmentNeedsAdjacentRefinementForReactant(reactant: ModelReactantMonomer)
    (implicit ev: ThisType <:< ModelStaticMonomer):
    Seq[(ModelStaticLocalEdgeSite, ModelObservingBoundEdgeSite)] = {

    ev(this).EdgeSites.zip(reactant.EdgeSites)
    // Only a local edge can be refined with a bound edge in this way, and they all need to be
    .collect{
      case (first: ModelStaticLocalEdgeSite, second: ModelObservingBoundEdgeSite) =>
        (first, second)
    }
  }

  def ProductNeedsAdjacentRefinementForFragment(fragment: ModelStaticMonomer)
    (implicit ev: ThisType <:< ModelReactantMonomer):
    Seq[(ModelStaticLocalEdgeSite, ModelStaticBoundEdgeSite)] = {

    ev(this).EdgeSites.zip(fragment.EdgeSites)
      // Only a local static edge site can be refined with a bound edge in this way, and they all need to be
      .collect{
      case (first: ModelStaticLocalEdgeSite, second: ModelStaticBoundEdgeSite) =>
        (first, second)
    }
  }

  def NeedsRemoteRefinementForContract(disjoint: ModelStaticGraph)
    (implicit evidence: EdgeSiteType <:< ModelReactantEdgeSite) = {

    // Examine the sites that can have ambiguity
    EdgeSites.collect{case x: ModelObservingLocalEdgeSite => x.asInstanceOf[EdgeSiteType with ModelObservingLocalEdgeSite]}
    // Check if this graph can be reached from this site
    .map(
      site => site.ReactantNeedsRemoteRefinementForContract(disjoint) match {
        case Some(distal_site) => Some((site, distal_site))
        case None => None
      }
    )
    // Flatten out the Nones
    .flatten
    // Return the first or None
    .headOption
  }

  def RefineCompartmentToward(
    refiner: ModelStaticCompartmentSite
  )(
    implicit evidence: CompartmentType <:< ModelReactantCompartmentSite
  ): ThisType = {
    val replacement_compartment = Compartment.RefineToward(refiner).asInstanceOf[CompartmentType]

    this.copy(Compartment = replacement_compartment)
  }

  def RefineCompartmentAgainst(
    refiner: ModelStaticCompartmentSite
  )(
    implicit evidence: CompartmentType <:< ModelReactantCompartmentSite
  ): ThisType = {
    val replacement_compartment = Compartment.RefineAgainst(refiner).asInstanceOf[CompartmentType]

    this.copy(Compartment = replacement_compartment)
  }

  def RefineEdgeSiteTowardLocal(
    site_to_refine: ModelObservingLocalEdgeSite,
    refiner: ModelStaticLocalEdgeSite
  )(
    implicit evidence: EdgeSiteType <:< ModelReactantEdgeSite
  ): ThisType = {
    // Find and refine edge
    val replacement_edges = EdgeSites.map{
      case `site_to_refine` => site_to_refine.RefineReactantTowardLocal(refiner).asInstanceOf[EdgeSiteType]
      case other => other
    }

    // Return updated monomer
    this.copy(EdgeSites = replacement_edges)
  }

  def RefineEdgeSiteAgainstLocal(
    site_to_refine: ModelObservingLocalEdgeSite,
    refiner: ModelStaticLocalEdgeSite
  )(
    implicit evidence: EdgeSiteType <:< ModelReactantEdgeSite
  ): ThisType = {
    // Find and refine edge
    val replacement_edges = EdgeSites.map{
      case `site_to_refine` => site_to_refine.RefineReactantAgainstLocal(refiner).asInstanceOf[EdgeSiteType]
      case other => other
    }

    // Return updated monomer
    this.copy(EdgeSites = replacement_edges)
  }

  def RefineEdgeSiteTowardBound(
    site_to_refine: ModelReactantEdgeSite with ModelObservingLocalEdgeSite,
    to_site: ModelAgentSite
  )(
    implicit evidence: EdgeSiteType <:< ModelReactantEdgeSite
  ): (ThisType, EdgeSiteType) = {
    val refined_edge_site = site_to_refine.RefineReactantTowardBound(to_site).asInstanceOf[EdgeSiteType]

    // Find and refine edge
    val refined_edge_sites: Seq[EdgeSiteType] = {
      EdgeSites.map{
        case x if (x == `site_to_refine`) => refined_edge_site // Evade stable type
        case other => other
      }
    }

    // Return
    (this.copy(EdgeSites = refined_edge_sites), refined_edge_site)
  }

  def RefineEdgeSiteAgainstBound(
    site_to_refine: ModelObservingLocalEdgeSite,
    to_site: ModelAgentSite
  )(implicit evidence: EdgeSiteType <:< ModelReactantEdgeSite): ThisType = {
    // Find and refine edge
    val replacement_edge_sites = EdgeSites.map{
      case `site_to_refine` => site_to_refine.RefineReactantAgainstBound(to_site).asInstanceOf[EdgeSiteType]
      case other => other
    }

    // Return updated monomer
    this.copy(EdgeSites = replacement_edge_sites)
  }


  def RefineStateSiteToward(
    site_to_refine: ModelReactantStateSite,
    refiner: ModelStaticStateSite
  )(
    implicit evidence: StateSiteType <:< ModelReactantStateSite
  ): ThisType = {
    // Find and refine edge
    val replacement_sites = StateSites.map{
      case x if x == `site_to_refine` => x.RefineReactantToward(refiner).asInstanceOf[StateSiteType]
      case other => other
    }

    // Return updated monomer
    this.copy(StateSites = replacement_sites)
  }

  def RefineStateSiteAgainst(
    site_to_refine: ModelReactantStateSite,
    refiner: ModelStaticStateSite
  )(
    implicit evidence: StateSiteType <:< ModelReactantStateSite
  ): ThisType = {
    // Find and refine edge
    val replacement_sites = StateSites.map{
      case x if x == `site_to_refine` => x.RefineReactantAgainst(refiner).asInstanceOf[StateSiteType]
      case other => other
    }

    // Return updated monomer
    this.copy(StateSites = replacement_sites)
  }

  def ToStaticReactant(implicit evidence: ThisType <:< ModelReactantMonomer): ModelStaticMonomer = {
    val new_this = evidence(this)

    ModelStaticMonomer(
      Agent = Agent,
      StateSites = new_this.StateSites.map(_.ToStaticReactant),
      EdgeSites = new_this.EdgeSites.map(_.ToStaticReactant),
      Compartment = new_this.Compartment.ToStaticReactant
    )
  }

  def ToStaticProduct(implicit evidence: ThisType <:< ModelProductMonomer): ModelStaticMonomer= {
    val new_this = evidence(this)

    ModelStaticMonomer(
      Agent = Agent,
      StateSites = new_this.StateSites.map(_.ToStaticProduct),
      EdgeSites = new_this.EdgeSites.map(_.ToStaticProduct),
      Compartment = new_this.Compartment.ToStaticProduct
    )
  }

  def IsConsumedBy(reactant_monomer: ModelReactantMonomer)(implicit evidence: ThisType <:< ModelStaticMonomer): Boolean = {
    val new_this = evidence(this)

    // Return
    val state_site_is_consumed =
      new_this.StateSites.zip(reactant_monomer.StateSites).view.exists(tuple => tuple._1.IsConsumedBy(tuple._2))

    val edge_site_is_consumed =
      new_this.EdgeSites.zip(reactant_monomer.EdgeSites).view.exists(tuple => tuple._1.IsConsumedBy(tuple._2))

    val compartment_is_consumed =
      new_this.Compartment.IsConsumedBy(reactant_monomer.Compartment)

    val do_not_disobey = !new_this.DisobeysContract(reactant_monomer.ToStaticReactant)

    // Return
    (state_site_is_consumed || edge_site_is_consumed || compartment_is_consumed) && do_not_disobey
  }

  def IsProducedBy(product_monomer: ModelProductMonomer)(implicit evidence: ThisType <:< ModelStaticMonomer): Boolean = {
    val new_this = evidence(this)

    val edge_site_is_produced =
      new_this.EdgeSites.zip(product_monomer.EdgeSites).view.exists(tuple => tuple._1.IsProducedBy(tuple._2))

    val state_site_is_produced =
      new_this.StateSites.zip(product_monomer.StateSites).view.exists(tuple => tuple._1.IsProducedBy(tuple._2))

    val compartment_is_produced =
      new_this.Compartment.IsProducedBy(product_monomer.Compartment)

    val do_not_disobey = !new_this.DisobeysContract(product_monomer.ToStaticProduct)

    // Return
    (edge_site_is_produced || state_site_is_produced || compartment_is_produced) && do_not_disobey
  }

  def ToStringWithEdgeNumbers(edge_number_map: Map[ModelEdgeSite,String]) = {
    val site_strings = {
      StateSites.zip(EdgeSites)
      .map(tuple => {
        val name = tuple._1.AgentSite.Name

        var state_site_is_totally_ambiguous = false

        val state_site = {
          tuple._1 match {
            case ModelStaticStateSite(_, static) if tuple._1.AgentSite.States.diff(static).isEmpty =>
              state_site_is_totally_ambiguous = true

              // Return
              static match {
                case Seq(ModelAgentState(_,StateName("0"))) => ""
                case Seq(single) => "~" + single
                case _ => "~?"
              }
            case ModelStaticStateSite(_, Seq(ModelAgentState(_,StateName("0")))) =>
              ""

            case ModelStaticStateSite(_, Seq(single)) =>
              "~" + single.Name

            case ModelStaticStateSite(_, static) =>
              "~" + static.mkString("{",",","}")
          }
        }

        val contacts_as_partial_edges = tuple._2.AgentSite.Edges.map(ModelPartialEdge(_)) :+ ModelUnboundEdge

        var edge_site_is_totally_ambiguous = false

        val edge_site = {
          tuple._2 match {
            case edge_site @ ModelStaticBoundEdgeSite(_,_) =>
              "-" + edge_number_map(edge_site)

            case ModelStaticLocalEdgeSite(_, static) if contacts_as_partial_edges.diff(static).isEmpty =>
              edge_site_is_totally_ambiguous = true

              // Return
              static match {
                case Seq(ModelUnboundEdge) => ""
                case Seq(single) => "-" + single
                case _ => "-?"
              }

            case ModelStaticLocalEdgeSite(_, Seq(ModelUnboundEdge)) =>
              ""

            case ModelStaticLocalEdgeSite(_, Seq(single)) =>
              "-" + single

            case ModelStaticLocalEdgeSite(_, static) =>
              "-" + static.mkString("{",",","}")
          }
        }

        // Return
        // Only return a string if the site is not completely ambiguous
        if (state_site_is_totally_ambiguous && edge_site_is_totally_ambiguous) {
          None
        }
        else {
          Some(name + state_site + edge_site)
        }
      })
      // Remove the totally ambiguous sites
      .flatten
    }

    // Return
    Agent.Name + site_strings.mkString("(",",",")") + Compartment
  }

  def MonomerCanExist(implicit ev: ThisType <:< ModelStaticMonomer): Boolean = {
    val new_this = ev(this)

    // Check individual states and edges
    val state_singles_can_exist = {
      new_this.StateSites.forall(site =>
        !site.Static.intersect(site.AgentSite.States).isEmpty
      )
    }

    val edge_singles_can_exist = {
      new_this.EdgeSites.forall(site =>
        !site.Observed.intersect(site.AgentSite.Edges.map(ModelPartialEdge(_)) :+ ModelUnboundEdge).isEmpty
      )
    }

    val state_site_doubles_can_exist = {
      new_this.StateSites.forall(site1 =>
        site1.Static.exists(state1 => {
          val state_sites_adjacent_to_state_sites = site1.AgentSite.StateSitesAdjacentToStateSites(state1)

          val all_other_state_sites_are_valid = {
            new_this.StateSites.filter(_ != site1).forall(site2 =>
              site2.Static.exists(state2 =>
                state_sites_adjacent_to_state_sites(site2.AgentSite).contains(state2)
              )
            )
          }

          val edge_sites_adjacent_to_state_sites = site1.AgentSite.EdgeSitesAdjacentToStateSites(state1)

          val all_other_edge_sites_are_valid = {
            new_this.EdgeSites.forall(site2 =>
              site2.Resulting.exists(state2 =>
                edge_sites_adjacent_to_state_sites(site2.AgentSite).contains(state2)
              )
            )
          }

          // Return
          all_other_state_sites_are_valid && all_other_edge_sites_are_valid
        })
      )
    }

    val edge_site_doubles_can_exist = {
      new_this.EdgeSites.forall(site1 =>
        site1.Resulting.exists(state1 => {
          val state_sites_adjacent_to_edge_sites = site1.AgentSite.StateSitesAdjacentToEdgeSites(state1)

          val all_other_state_sites_are_valid = {
            new_this.StateSites.forall(site2 =>
              site2.Static.exists(state2 =>
                state_sites_adjacent_to_edge_sites(site2.AgentSite).contains(state2)
              )
            )
          }

          val edge_sites_adjacent_to_edge_sites = site1.AgentSite.EdgeSitesAdjacentToEdgeSites(state1)

          val all_other_edge_sites_are_valid = {
            new_this.EdgeSites.filter(_ != site1).forall(site2 =>
              site2.Resulting.exists(state2 =>
                edge_sites_adjacent_to_edge_sites(site2.AgentSite).contains(state2)
              )
            )
          }

          // Return
          all_other_state_sites_are_valid && all_other_edge_sites_are_valid
        })
      )
    }

    // Return
    state_singles_can_exist && edge_singles_can_exist && state_site_doubles_can_exist && edge_site_doubles_can_exist
  }

  def IsModelStaticMonomer = (
    manifest_types._1 <:< manifest[ModelStaticStateSite] &&
    manifest_types._2 <:< manifest[ModelStaticEdgeSite] &&
    manifest_types._3 <:< manifest[ModelStaticCompartmentSite]
  )

  def IsModelProducedMonomer = (
    manifest_types._1 <:< manifest[ModelProducedStateSite] &&
    manifest_types._2 <:< manifest[ModelProducedEdgeSite] &&
    manifest_types._3 <:< manifest[ModelProducedCompartmentSite]
  )

  def IsModelConsumedMonomer = (
    manifest_types._1 <:< manifest[ModelConsumedStateSite] &&
    manifest_types._2 <:< manifest[ModelConsumedEdgeSite] &&
    manifest_types._3 <:< manifest[ModelConsumedCompartmentSite]
  )

  def IsModelConservedMonomer = (
    manifest_types._1 <:< manifest[ModelConservedStateSite] &&
    manifest_types._2 <:< manifest[ModelConservedEdgeSite] &&
    manifest_types._3 <:< manifest[ModelConservedCompartmentSite]
  )

  def IsModelReactantMonomer = (
    manifest_types._1 <:< manifest[ModelReactantStateSite] &&
    manifest_types._2 <:< manifest[ModelReactantEdgeSite] &&
    manifest_types._3 <:< manifest[ModelReactantCompartmentSite]
  )

  def IsModelProductMonomer = (
    manifest_types._1 <:< manifest[ModelProductStateSite] &&
    manifest_types._2 <:< manifest[ModelProductEdgeSite] &&
    manifest_types._3 <:< manifest[ModelProductCompartmentSite]
  )

  override def toString =
    (this match {
      case x if x.IsModelStaticMonomer => x.asInstanceOf[ModelStaticMonomer]
      case x if x.IsModelReactantMonomer => x.asInstanceOf[ModelReactantMonomer].ToStaticReactant
      case x if x.IsModelProductMonomer => x.asInstanceOf[ModelProductMonomer].ToStaticProduct
    })
    .ToStringWithEdgeNumbers(Map().withDefaultValue("#"))
}

object ModelConsumedMonomer {
  def apply(
    original: RuleMonomer,
    compartments: Seq[ModelCompartment],
    agents: Seq[ModelAgent],
    edge_map: Map[Int,Buffer[(ModelEdgeSite,Int,Int)]]
  ): ModelConsumedMonomer = {
    // Find agent
    val agent = agents.FindOrDie(_.Name == original.Name)

    // Resolve compartments
    val compartment_site = ModelConsumedCompartmentSite(original.Compartment, compartments)

    // Build sites
    val state_sites = {
      for (site <- original.Sites) yield {
        ModelConsumedStateSite(site.State, site.Name, agent, compartments, agents)
      }
    }

    val edge_sites = {
      for (i_site <- 0 until original.Sites.size) yield {
        val site = original.Sites(i_site)
        ModelConsumedEdgeSite(site.Edge, site.Name, agent, original.Index.get, i_site, agents, edge_map)
      }
    }

    // Return
    ModelMonomerClass(agent, state_sites, edge_sites, compartment_site)
  }
}

object ModelProducedMonomer {
  def apply(
    original: RuleMonomer,
    compartments: Seq[ModelCompartment],
    agents: Seq[ModelAgent],
    edge_map: Map[Int,Buffer[(ModelEdgeSite,Int,Int)]]
  ): ModelProducedMonomer = {
    // Find agent
    val agent = agents.FindOrDie(_.Name == original.Name)

    // Resolve compartments
    val compartment_site = ModelProducedCompartmentSite(original.Compartment, compartments)

    // Build sites
    val state_sites = {
      for (site <- original.Sites) yield {
        ModelProducedStateSite(site.State, site.Name, agent, compartments, agents)
      }
    }

    val edge_sites = {
      for (i_site <- 0 until original.Sites.size) yield {
        val site = original.Sites(i_site)
        ModelProducedEdgeSite(site.Edge, site.Name, agent, original.Index.get, i_site, agents, edge_map)
      }
    }

    // Return
    ModelMonomerClass(agent, state_sites, edge_sites, compartment_site)
  }
}

object ModelConservedMonomer {
  def apply(
    Agent: ModelAgent,
    StateSites: Seq[ModelConservedStateSite],
    EdgeSites: Seq[ModelConservedEdgeSite],
    Compartment: ModelConservedCompartmentSite
  ): ModelConservedMonomer = new ModelMonomerClass[ModelConservedStateSite,ModelConservedEdgeSite,ModelConservedCompartmentSite](
    Agent,
    StateSites,
    EdgeSites,
    Compartment
  )

  def apply(
    Agent: ModelAgent,
    StateSites: Option[Seq[ModelConservedStateSite]] = None,
    EdgeSites: Option[Seq[ModelConservedEdgeSite]] = None,
    Compartment: Option[ModelConservedCompartmentSite] = None
  ): ModelConservedMonomer = {
    val state_sites = StateSites match {
      case Some(given_sites) => Agent.Sites.map{site =>
        val matching_site = given_sites.find(_.AgentSite == site)

        matching_site match {
          case Some(original) => original
          case None           => ModelStaticStateSite(site)
        }
      }
      case None => Agent.Sites.map(ModelStaticStateSite(_))
    }

    val edge_sites = EdgeSites match {
      case Some(given_sites) => Agent.Sites.map{site =>
        val matching_site = given_sites.find(_.AgentSite == site)

        matching_site match {
          case Some(original) => original
          case None           => ModelStaticEdgeSite(site)
        }
      }
      case None => Agent.Sites.map(ModelStaticEdgeSite(_))
    }

    val compartment = Compartment.getOrElse(ModelStaticCompartmentSite(Agent.Compartments))

    // Return
    ModelConservedMonomer(Agent, state_sites, edge_sites, compartment)
  }

  def apply(
    original: RuleMonomer,
    compartments: Seq[ModelCompartment],
    agents: Seq[ModelAgent],
    edge_map: Map[Int,Buffer[(ModelEdgeSite,Int,Int)]]
  ): ModelConservedMonomer = {
    // Find agent
    val agent = agents.FindOrDie(_.Name == original.Name)

    // Resolve compartments
    val compartment_site = ModelStaticCompartmentSite(original.Compartment, compartments)

    // Build sites
    val state_sites = {
      for (site <- original.Sites) yield {
        ModelStaticStateSite(site.State, site.Name, agent, compartments, agents)
      }
    }

    val edge_sites = {
      for (i_site <- 0 until original.Sites.size) yield {
        val site = original.Sites(i_site)
        ModelConservedEdgeSite(site.Edge, site.Name, agent, original.Index.get, i_site, agents, edge_map)
      }
    }

    // Return
    ModelMonomerClass[ModelStaticStateSite,ModelStaticEdgeSite,ModelStaticCompartmentSite](agent, state_sites, edge_sites, compartment_site)
  }
}

object ModelStaticMonomer {
  def apply(
    Agent: ModelAgent,
    StateSites: Seq[ModelStaticStateSite],
    EdgeSites: Seq[ModelStaticEdgeSite],
    Compartment: ModelStaticCompartmentSite
  ): ModelStaticMonomer = new ModelMonomerClass[ModelStaticStateSite,ModelStaticEdgeSite,ModelStaticCompartmentSite](
    Agent,
    StateSites,
    EdgeSites,
    Compartment
  )

  def apply(
    Agent: ModelAgent,
    StateSites: Option[Seq[ModelStaticStateSite]] = None,
    EdgeSites: Option[Seq[ModelStaticEdgeSite]] = None,
    Compartment: Option[ModelStaticCompartmentSite] = None
  ): ModelStaticMonomer = {
    val state_sites = StateSites match {
      case Some(given_sites) => Agent.Sites.map{site =>
        val matching_site = given_sites.find(_.AgentSite == site)

        matching_site match {
          case Some(original) => original
          case None           => ModelStaticStateSite(site)
        }
      }
      case None => Agent.Sites.map(ModelStaticStateSite(_))
    }

    val edge_sites = EdgeSites match {
      case Some(given_sites) => Agent.Sites.map{site =>
        val matching_site = given_sites.find(_.AgentSite == site)

        matching_site match {
          case Some(original) => original
          case None           => ModelStaticEdgeSite(site)
        }
      }
      case None => Agent.Sites.map(ModelStaticEdgeSite(_))
    }

    val compartment = Compartment.getOrElse(ModelStaticCompartmentSite(Agent.Compartments))

    // Return
    ModelStaticMonomer(Agent, state_sites, edge_sites, compartment)
  }

  def apply(
    original: Monomer,
    compartments: Seq[ModelCompartment],
    agents: Seq[ModelAgent],
    edge_map: Map[Int,Buffer[ModelEdgeSite]]
  ): ModelStaticMonomer = {
    // Find agent
    val agent = agents.FindOrDie(_.Name == original.Name)

    // Resolve compartments
    val compartment_site = ModelStaticCompartmentSite(original.Compartment, compartments)

    // Build sites
    val state_sites = {
      for (site <- original.Sites) yield {
        ModelStaticStateSite(site.State, site.Name, agent, compartments, agents)
      }
    }

    val edge_sites = {
      for (i_site <- 0 until original.Sites.size) yield {
        val site = original.Sites(i_site)
        ModelStaticEdgeSite(site.Edge, site.Name, agent, agents, edge_map)
      }
    }

    // Return
    ModelStaticMonomer(agent, state_sites, edge_sites, compartment_site)
  }
}
