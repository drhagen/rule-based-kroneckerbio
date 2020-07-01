/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

import scala.collection.mutable.{ArrayBuffer,Buffer}
import kroneckerbio.model._
import kroneckerbio.model.refining.refining._
import kroneckerbio.utilities.Inquirable._
import kroneckerbio.utilities.RichSeq._
import kroneckerbio.implicitOption
import kroneckerbio.utilities.EqualsByReference
import kroneckerbio.utilities.RichAny._

final case class ModelGraphClass[
  +MonomerType <: ModelMonomer : Manifest,
  +EdgeType <: ModelBoundEdge : Manifest
](
  Monomers: Seq[MonomerType],
  Edges: Seq[EdgeType]
) extends EqualsByReference {

  type ThisType = ModelGraphClass[MonomerType,EdgeType]

  def manifest_types = (implicitly[Manifest[_ <: MonomerType]], implicitly[Manifest[_ <: EdgeType]])

  //////
  // Methods for setting the bound edge sites to the correct type
  //////
  // Rebuild edge sites to be consistent with bound edges
  def BoundEdgeSitesRebuilt(
    reactant_insertion_map: Map[ModelEdgeSite, ModelPartialEdge] = ReactantEdgeTargetTypeMap,
    produced_insertion_map: Map[ModelEdgeSite, ModelPartialEdge] = ProducedEdgeTargetTypeMap
  ): ThisType = {
    // Rebuild monomers with new edge sites
    val monomers =
      Monomers.map(_.BoundEdgeSitesRebuilt(reactant_insertion_map, produced_insertion_map).asInstanceOf[MonomerType])

    // Align old sites with new sites to create a map for the edges
    val edge_site_map = Monomers.flatMap(_.EdgeSites).zip(monomers.flatMap(_.EdgeSites)).toMap

    // Rebuild the edges with new targets
    val edges = {
      Edges.map(edge =>
        edge.pipe{
            // Could use a case trait copy...
            case ModelConsumedBoundEdge(targets) => ModelConsumedBoundEdge(targets.map(edge_site_map(_)))
            case ModelProducedBoundEdge(targets) => ModelProducedBoundEdge(targets.map(edge_site_map(_)))
            case ModelStaticBoundEdge(targets)   => ModelStaticBoundEdge(targets.map(edge_site_map(_)))
        }
        .asInstanceOf[EdgeType]
      )
    }

    // Return graph with rebuilt monomers and edges
    ModelGraphClass(monomers, edges)
  }

  def ReactantEdgeTargetTypeMap = {
    (for (edge <- Edges.collect{case x: ModelReactantBoundEdge => x}) yield {
      val partial_edges = edge.Targets.map(target => ModelPartialEdge(target.AgentSite))

      // Return arrow
      Seq(edge.Targets(0) -> partial_edges(1), edge.Targets(1) -> partial_edges(0))
    })
    .flatten
    .toMap
  }

  def ProducedEdgeTargetTypeMap = {
    (for (edge <- Edges.collect{case x: ModelProducedBoundEdge => x}) yield {
      val partial_edges = edge.Targets.map(target => ModelPartialEdge(target.AgentSite))

      // Return arrow
      Seq(edge.Targets(0) -> partial_edges(1), edge.Targets(1) -> partial_edges(0))
    })
    .flatten
    .toMap
  }

  //////
  // Methods for agent reduction
  //////
  def AllStaticMonomerNames: Seq[ModelAgent] =
    Monomers.filter(_.IsModelStaticMonomer).map(_.Agent)

  def AllProducedMonomerNames: Seq[ModelAgent] =
    Monomers.filter(_.IsModelProducedMonomer).map(_.Agent)

  def AllStaticMonomerCompartmentNames: Seq[(ModelAgent,ModelCompartment)] = {
    for {
      monomer <- Monomers
      if monomer.Compartment.isInstanceOf[ModelStaticCompartmentSite]
      compartment_site = monomer.Compartment.asInstanceOf[ModelStaticCompartmentSite]
      compartment <- compartment_site.Static
    } yield {
      (monomer.Agent, compartment)
    }
  }

  def AllProducedMonomerCompartmentNames: Seq[(ModelAgent,ModelCompartment)] = {
    for {
      monomer <- Monomers
      if monomer.Compartment.isInstanceOf[ModelProducingCompartmentSite]
      compartment_site = monomer.Compartment.asInstanceOf[ModelProducingCompartmentSite]
      compartment <- compartment_site.Produced
    } yield {
      (monomer.Agent, compartment)
    }
  }

  def AllStaticMonomerSiteStateNames: Seq[(ModelAgent,ModelAgentSite,ModelAgentState)] = {
    for {
      monomer <- Monomers
      state_site <- monomer.StateSites.collect{case x: ModelStaticStateSite => x}
      state <- state_site.Static
    } yield {
      (monomer.Agent, state_site.AgentSite, state)
    }
  }

  def AllProducedMonomerSiteStateNames: Seq[(ModelAgent,ModelAgentSite,ModelAgentState)] = {
    for {
      monomer <- Monomers
      state_site <- monomer.StateSites.collect{case x: ModelProducingStateSite => x}
      state <- state_site.Produced
    } yield {
      (monomer.Agent, state_site.AgentSite, state)
    }
  }

  def AllStaticBoundEdgeSitePairNames: Seq[Set[(ModelAgent,ModelAgentSite)]] = {
    Edges.collect{case x: ModelStaticBoundEdge => x}.map(extract_monomer_site_names(_).toSet)
  }

  def AllProducedBoundEdgeSitePairNames: Seq[Set[(ModelAgent,ModelAgentSite)]] = {
    Edges.collect{case x: ModelProducedBoundEdge => x}.map(extract_monomer_site_names(_).toSet)
  }

  private def extract_monomer_site_names(edge: ModelBoundEdge) = {
    for {
      // Search every monomer and site
      monomer <- Monomers
      site <- monomer.EdgeSites

      // Keep it if it matches one of the targets
      if edge.Targets.contains(site)
    } yield {
      // Return only the names
      (monomer.Agent, site.AgentSite)
    }
  }

  def Reduced(
    edge_site_tuples: Buffer[(ModelEdgeSite,ModelEdgeSite)] = Buffer[(ModelEdgeSite,ModelEdgeSite)]()
  ): ThisType = {
    val monomers = Monomers.map(_.Reduced(edge_site_tuples))

    // Remap reactant edges
    val edge_site_map = edge_site_tuples.toMap.withDefault(identity(_))

    val edges = {
      Edges.map{
        // Could use a case trait copy...
        case ModelConsumedBoundEdge(targets) => ModelConsumedBoundEdge(targets.map(edge_site_map(_)))
        case ModelProducedBoundEdge(targets) => ModelProducedBoundEdge(targets.map(edge_site_map(_)))
        case ModelStaticBoundEdge(targets)   => ModelStaticBoundEdge(targets.map(edge_site_map(_)))
      }
    }

    // Return
    ModelGraphClass(monomers.asInstanceOf[Seq[MonomerType]], edges.asInstanceOf[Seq[EdgeType]])
  }

  /////
  // Useful information methods
  /////
  def IsStaticGraph =
    manifest_types._1 <:< manifest[ModelStaticMonomer] && manifest_types._2 <:< manifest[ModelStaticBoundEdge]

  def IsReactantGraph =
    manifest_types._1 <:< manifest[ModelReactantMonomer] && manifest_types._2 <:< manifest[ModelReactantBoundEdge]

  def IsProductGraph =
    manifest_types._1 <:< manifest[ModelProductMonomer] && manifest_types._2 <:< manifest[ModelProductBoundEdge]

  // Simple find method for monomers that own a particular edge site
  def MonomerWithEdgeSite(site: ModelEdgeSite): MonomerType =
    Monomers.FindOrDie(monomer => monomer.EdgeSites.contains(site))

  // What are the indexes in monomers and sites for a particular site, if any
  def EdgeSiteIndex(site: ModelEdgeSite): Option[(Int,Int)] = {
    Monomers.view.zipWithIndex
    .map(tuple => (tuple._2, tuple._1.EdgeSiteIndex(site)))
    .find(_._2 != None)
    match {
      case Some((monomer_index, Some(site_index))) => Some(monomer_index, site_index)
      case _ => None
    }
  }

  // Collect the targets of all the partial edges
  def AmbiguousTargetSites(implicit evidence: MonomerType <:< ModelReactantMonomer): Seq[ModelAgentSite] =
    Monomers.flatMap(monomer => evidence(monomer).AmbiguousTargetSites)

  // Which monomers are reachable through those sites
  def ReachableMonomers(implicit evidence: MonomerType <:< ModelReactantMonomer): Seq[ModelAgent] =
    AmbiguousTargetSites.flatMap(_.EntryReachableMonomers).distinct

  // Give me the monomers that are adjacent to this monomer
  def AdjacentMonomers(monomer: ModelMonomer): Seq[MonomerType] = {
    val edges_connected = Edges.filter(edge => edge.Targets.exists(target => monomer.EdgeSites.contains(target)))

    val sites_connected = edges_connected.map(_.Targets).flatten

    // Return
    Monomers
    .filter(monomer => monomer.EdgeSites.exists(site => sites_connected.contains(site)))
    .distinct
  }

  // The possible compartments of a graph are (1) mentioned as a possible site on a local or distal monomer,
  // (2) not larger than the smallest dimension possible for all local monomers, (3) adjacent to a compartment
  // sharing the smallest dimension possible for all monomers
  def PossibleGraphCompartments(implicit ev: ThisType <:< ModelStaticGraph): Seq[ModelCompartment] = {
    // Get the possible compartments according to the local and distal monomers
    val local_compartment_sites = ev(this).Monomers.map(_.Compartment.Static)
    val distant_compartment_sites = ev(this).ReachableMonomers.map(_.Compartments)

    // Flattened compartments
    val local_compartments = local_compartment_sites.flatten.distinct
    val distal_compartments = distant_compartment_sites.flatten.distinct

    // No graph compartment can be larger than the smallest dimension possible for all monomers
    val max_local_dimensions = local_compartment_sites.map(_.map(_.Dimension).max)
    val minimax_dimension = max_local_dimensions.min

    // Graph compartments that allowed by the local configuration
    val local_graph_compartments = local_compartments.filter(_.Dimension <= minimax_dimension)

    // Graph compartments that also possible because this graph could connect to a distant monomer
    // These can only be compartments smaller than the minimax dimension because a larger compartment
    //   would not be allowed, and two compartments of the same size cannot be adjacent
    val distal_graph_compartments = {
      distal_compartments
      .filter(_.Dimension < minimax_dimension)
      .filter(!_.AdjacentCompartments.intersect(local_graph_compartments).isEmpty)
    }

    // Return
    (local_graph_compartments ++ distal_graph_compartments).distinct
  }

  def ToReactant: ModelReactantGraph = {
    val monomers = {
      Monomers
      .filter(_.IsModelReactantMonomer)
      .map(_.asInstanceOf[ModelReactantMonomer])
    }

    val edges = Edges.collect{case x: ModelReactantBoundEdge => x}

    // Return
    ModelGraphClass(monomers, edges)
  }

  def ToProduct: ModelProductGraph = {
    val monomers = {
      Monomers
      .filter(_.IsModelProductMonomer)
      .map(_.asInstanceOf[MonomerType with ModelProductMonomer])
    }

    val edges = Edges.collect{case x: EdgeType with ModelProductBoundEdge => x}

    // Return
    ModelGraphClass(monomers, edges)
  }

  def ToStaticReactant: ModelStaticGraph = {
    val monomer_pairs = {
      Monomers
      .filter(_.IsModelReactantMonomer)
      .map(_.asInstanceOf[ModelReactantMonomer])
      .map(monomer => (monomer, monomer.ToStaticReactant))
    }

    val reactant_edges = Edges.collect{case x: ModelReactantBoundEdge => x}

    // Map of old sites to new sites
    val site_map = (
      monomer_pairs
      .map(pair => (pair._1.EdgeSites, pair._2.EdgeSites))
      .unzip
      .pipe(pair => (pair._1.flatten, pair._2.flatten))
      .zipped
      .toMap[ModelEdgeSite,ModelEdgeSite]
    )

    // Relink edges
    val edges = relink_static_edges(reactant_edges, site_map)

    // Return
    ModelGraphClass(monomer_pairs.map(_._2), edges)
  }

  def ToStaticProduct: ModelStaticGraph = {
    val monomer_pairs = {
      Monomers
      .filter(_.IsModelProductMonomer)
      .map(_.asInstanceOf[ModelProductMonomer])
      .map(monomer => (monomer, monomer.ToStaticProduct))
    }

    val product_edges = Edges.collect{case x: ModelProductBoundEdge => x}

    // Map of old sites to new sites
    val site_map = (
      monomer_pairs
      .map(pair => (pair._1.EdgeSites, pair._2.EdgeSites))
      .unzip
      .pipe(pair => (pair._1.flatten, pair._2.flatten))
      .zipped
      .toMap[ModelEdgeSite,ModelEdgeSite]
    )

    // Relink edges
    val edges = relink_static_edges(product_edges, site_map)

    // Return
    ModelGraphClass(monomer_pairs.map(_._2), edges)
  }

  private def relink_static_edges(edges: Seq[ModelBoundEdge], site_map: Map[ModelEdgeSite,ModelEdgeSite]) = {
    edges
    .map(edge =>
      edge.Targets
      .map(target => site_map(target))
      .pipe(targets => ModelStaticBoundEdge(targets))
    )
  }

  private def remove_unlinked_edges: ThisType = {
    // Keep only edges that have two links to current monomers
    val new_edges = Edges.filter(_.Targets.flatMap(EdgeSiteIndex(_)).size == 2)

    // Return the graph reconstructed
    ModelGraphClass(Monomers, new_edges)
  }

  def SplitIntoConnected: Seq[ThisType] = {
    // Place to store each disconnected graph
    var remaining = Monomers
    var graphs = Seq[Seq[MonomerType]]()

    while (remaining.size > 0) {
      // Place to store found monomers; start with first in remaining
      var monomers_found = Seq(remaining(0)) // Don't repeat ourselves
      var next_monomers = AdjacentMonomers(remaining(0))

      while (next_monomers.size > 0) {
        // Find the monomers adjacent to the ones we just found, but not ones we have already found
        val adjacent_monomers = next_monomers.flatMap(AdjacentMonomers(_)).distinct
        next_monomers = adjacent_monomers.diff(monomers_found)

        // Add them to the list of monomers found
        monomers_found ++= next_monomers
      }

      // Add this group to the list of graphs
      graphs :+= monomers_found

      // Subtract the found monomers from the list of remaining monomers
      remaining = remaining.diff(monomers_found)
    }

    // Map sites to edges
    val map_site_to_edge = (
      Edges
      .map(edge =>
        Seq((edge.Targets(0),edge),(edge.Targets(1),edge))
      )
      .flatten
      .toMap
    )

    // Collect edges appropriate to each graph
    val edges = (
      graphs
      .map(graph =>
        // Get all sites on graph
        graph.map(_.EdgeSites)
        .flatten
        // Look up each site to get the corresponding edge
        .map(site =>
          map_site_to_edge.get(site)
        )
        .flatten
        .distinct
      )
    )

    // Return
    graphs.zip(edges).map(pair => ModelGraphClass(pair._1, pair._2))
  }

  // Sometimes, refining a site will end up with an empty site. That means that this pattern will never match any
  // species. Therefore, it should be deleted.
  def GraphCanExist(implicit ev: ThisType <:< ModelStaticGraph): Boolean = ev(this).Monomers.forall(_.MonomerCanExist)

  /////
  // Disjoint matching
  /////
  // Does a graph match a species represented by this graph, possibly match it, or definitely not match it
  def SimplifyDisjoint(disjoint: ModelStaticGraph)(implicit evidence: ThisType <:< ModelReactantGraph) = {
    val new_this = evidence(this)

    val result = disjoint match {
      case _ if new_this.ToStaticReactant.ObeysContract(disjoint) => True
      case _ if new_this.ToStaticReactant.DisobeysContract(disjoint) => False
      case _ => InquirableLeaf(disjoint)
    }

    // Return
    result
  }

  /////
  // Graph alignment methods
  /////
  // Finds all subset permutations of this.Monomers that match the supplied contract
  def FullAlignments(contract: ModelStaticGraph)(implicit ev: ThisType <:< ModelStaticGraph):
  Seq[Seq[ModelStaticMonomer]] = {

    if (contract.Monomers.size > Monomers.size || contract.Edges.size > Edges.size) {
      Nil
    } else {
      // Define recursive function
      def align(attempt: Seq[ModelStaticMonomer]):
        Seq[Seq[ModelStaticMonomer]] = {

        if (contract.Monomers.size == attempt.size) {
          // All monomers are valid, check the edges
          // For all edges in contract, there must be an edge that points to the same monomer-index and
          // site-index for both target sites

          // Create temporary graph to gain access edge indexing methods
          val attempt_graph = ModelGraphClass(attempt, Edges)

          val contract_edge_indexes =
            contract.Edges.map(_.Targets.flatMap(contract.EdgeSiteIndex(_)).toSet) // FlatMap should see no Nones

          val attempt_edge_indexes =
            attempt_graph.Edges.map(_.Targets.flatMap(attempt_graph.EdgeSiteIndex(_)).toSet) // FlatMap may see Nones

          // Every contract.Edges is also in this.Edges
          val edges_all_match = contract_edge_indexes.diff(attempt_edge_indexes).isEmpty

          // Return
          if (edges_all_match) {
            Seq(attempt)
          } else {
            Nil
          }
        } else {
          // Add another monomer, check it, then recurse
          val monomer_index = attempt.size // Already has the +1 for 0-indexing

          // Search all monomers that could go into the next position
          //   They are not already in the matching graph and match the next position
          val valid_next_monomers = {
            ev(this).Monomers.filter(
              monomer => !attempt.contains(monomer) && monomer.ObeysContract(contract.Monomers(monomer_index))
            )
          }

          // Return recursively
          valid_next_monomers.flatMap(monomer => align(attempt :+ monomer))
        }
      }

      // Spawn recursive search for a way to match the reactant to the disjoint
      align(Nil)
    }
  }

  def ObeysContract(contract: ModelStaticGraph)(implicit ev: ThisType <:< ModelStaticGraph): Boolean = {
    !ev(this).FullAlignments(contract).isEmpty
  }

  // Is the supplied graph inconsistent with this graph's ambiguity
  def DisobeysContract(contract: ModelStaticGraph)
    (implicit evidence: MonomerType <:< ModelStaticMonomer): Boolean = {

    //#TODO: Overcome this limitation to allow for more complex disjoint
    assert(contract.Monomers.size == 1)

    // Does not meet the contract locally
    val local = Monomers.forall(monomer => evidence(monomer).DisobeysContract(contract.Monomers(0)))
    // Or through any other reachable monomers
    val remote = ReachableMonomers.forall(
      agent =>
        ModelStaticMonomer(agent).DisobeysContract(contract.Monomers(0))
    )

    local && remote
  }

  def IdenticalContract(contract: ModelStaticGraph
    )(implicit ev: ThisType <:< ModelStaticGraph): Boolean = {
    this.ObeysContract(contract) && contract.ObeysContract(this)
  }

  def AutomorphismCount(implicit ev: ThisType <:< ModelStaticGraph): Int = {
    ev(this).AlignmentCount(this)
  }

  def AlignmentCount(contract: ModelStaticGraph)(implicit ev: ThisType <:< ModelStaticGraph) = {
    ev(this).FullAlignments(contract).size
  }

  // The number of times that the supplied contract matches this
  def MatchesCount(contract: ModelStaticGraph)(implicit ev: ThisType <:< ModelStaticGraph) = {
    AlignmentCount(contract: ModelStaticGraph) / contract.AutomorphismCount
  }

  // The number of times that this as a reactant consumes a fragment
  def ConsumedCount(fragment: ModelStaticGraph): Int = {
    val full_alignments = fragment.FullAlignments(ToStaticReactant)

    // Filter on alignments that have at least one consumed monomer
    val consumed_alignments = {
      full_alignments.filter(fragment_monomers =>
        fragment_monomers.zip(ToReactant.Monomers)
        .exists(tuple => tuple._1.IsConsumedBy(tuple._2))
      )
    }

    // Return count
    consumed_alignments.size / ToStaticReactant.AutomorphismCount
  }

  // The number of times that this as a product produces a fragment
  def ProducedCount(fragment: ModelStaticGraph): Int = {
    // Because only a static graph can be aligned, the product must be converted to static before it can be aligned
    // with the fragment. However, the static monomers cannot determine if they actually produce the fragment monomers
    // so a map from the static monomers to the product monomers must be made to regenerate the
    val product = ToProduct
    val static_product = ToStaticProduct

    val static_product_map = static_product.Monomers.zip(product.Monomers).toMap

    val full_alignments = static_product.FullAlignments(fragment)

    // Filter on alignments that have at least one consumed monomer
    val produced_alignments = {
      full_alignments.filter(aligned_monomers =>
        // Zip the fragment monomers with the regenerated product monomers
        fragment.Monomers.zip(aligned_monomers.map(static_product_map(_)))
        // Check if at least one monomer has something that was produced
        .exists(tuple => tuple._1.IsProducedBy(tuple._2))
      )
    }

    // Return count
    produced_alignments.size / fragment.AutomorphismCount
  }

  def IsConsuming(fragment: ModelStaticGraph): Boolean = {
    ConsumedCount(fragment) > 0
  }

  def IsProducing(fragment: ModelStaticGraph): Boolean = {
    ProducedCount(fragment) > 0
  }

  /////
  // Refinement methods
  /////
  // Refinable monomers
  private def refinable_monomers: Seq[MonomerType with ModelReactantMonomer] = {
    Monomers.filter(_.IsModelReactantMonomer).asInstanceOf[Seq[MonomerType with ModelReactantMonomer]]
  }

  // What sub graph will have to be refined in order to meet this contract
  def ReactantNeedsLocalRefinementForContract(
    contract: ModelStaticGraph
  )(implicit ev: ThisType <:< ModelReactantGraph): (
      Seq[(ModelReactantCompartmentSite, ModelStaticCompartmentSite)],
      Seq[(ModelReactantStateSite, ModelStaticStateSite)],
      Seq[(ModelReactantEdgeSite with ModelObservingLocalEdgeSite, ModelStaticEdgeSite with ModelObservingLocalEdgeSite)]
   ) = {
    //#TODO: Overcome this limitation
    assert(contract.Monomers.size == 1)

    val refinables = {
      ev(this).Monomers
      // Extract the local changes that need to be made to make this graph match the contract
      .map(_.ReactantNeedsLocalRefinementForContract(contract.Monomers(0)))
      // Return only the first one
      .find(tuple => tuple != (None,Nil,Nil))
    }

    // Return
    refinables match {
      case Some(refinable) => (refinable._1.toSeq, refinable._2, refinable._3)
      case None            => (Nil,Nil,Nil)
    }
  }

  // What site will have to be refined to get closer to this contract
  def ReactantNeedsRemoteRefinementForContract(
    contract: ModelStaticGraph
  )(implicit ev: MonomerType <:< ModelReactantMonomer) = {
    Monomers.map(monomer => ev(monomer).NeedsRemoteRefinementForContract(contract)).flatten.headOption
  }

  // Refines a specific site to all sites matching a particular agent site
  def RefineEdgeSiteTowardBoundMonomer(
    site_to_refine: ModelObservingLocalEdgeSite,
    to_site: ModelAgentSite
  ): Seq[ThisType] = {
    // Monomer to be refined
    val from_monomer = refinable_monomers.FindOrDie(_.EdgeSites.contains(site_to_refine))

    // From site
    val from_site = site_to_refine.AgentSite

    // Replace the from monomer
    val (replacement_monomer, from_binding_site) = from_monomer.RefineEdgeSiteTowardBound(site_to_refine, to_site)
    val replacement_monomers = {
      Monomers.map{
        case x if x == `from_monomer` => replacement_monomer.asInstanceOf[MonomerType with ModelReactantMonomer]
        case other => other
      }
    }

    // Find all internal binding sites where this site can connect to
    val internally_bound_graphs = {
      for {
        // Loop over every monomer
        i_monomer <- 0 until replacement_monomers.size

        // Only keep the reactant monomers, which are refinable
        if replacement_monomers(i_monomer).IsModelReactantMonomer
        to_monomer = replacement_monomers(i_monomer).asInstanceOf[MonomerType with ModelReactantMonomer]

        // Only keep the monomers that match the agent of the to-site
        if to_monomer.Agent == to_site.Parent

        // Loop over every ambiguous monomer site
        site <- to_monomer.EdgeSites.collect{case x: ModelObservingLocalEdgeSite => x}

        // Only keep the sites that match the site of the to-site
        if site.AgentSite == to_site

        // Only proceed if this site can connect to the from-site
        if site.AllowsNewBindingTo(from_site)
      } yield {
        // Create a new monomer with the to-site replaced
        val (replacement_to_monomer, to_binding_site) = to_monomer.RefineEdgeSiteTowardBound(site_to_refine, from_site)

        // Update monomer vector with new monomer
        val complete_internal_monomers =
          replacement_monomers.updated(i_monomer, replacement_to_monomer.asInstanceOf[MonomerType])

        // Create new edge based on new sites
        val new_edge = ModelStaticBoundEdge(Seq(from_binding_site, to_binding_site))

        // Update edges to include the new edge
        val complete_internal_edges = Edges :+ new_edge.asInstanceOf[EdgeType]

        // Return new graph with all components
        copy(complete_internal_monomers, complete_internal_edges)
      }
    }

    // Create the one externally bound graph
    val (external_monomer, new_edge) = {
      site_to_refine match {
        case _: ModelStaticLocalEdgeSite =>
          val to_binding_site = ModelStaticBoundEdgeSite(to_site, ModelPartialEdge(from_site))

          val external_monomer = ModelStaticMonomer(to_site.Parent, EdgeSites = Seq(to_binding_site))

          val new_edge = ModelStaticBoundEdge(Seq(from_binding_site, to_binding_site))

          (external_monomer, new_edge)

        case _: ModelConsumingLocalEdgeSite =>
          val to_binding_site = ModelChangedBoundUnboundEdgeSite(to_site, ModelPartialEdge(from_site))

          val external_monomer = ModelConservedMonomer(to_site.Parent, EdgeSites = Seq(to_binding_site))

          val new_edge = ModelConsumedBoundEdge(Seq(from_binding_site, to_binding_site))

          (external_monomer, new_edge)
      }
    }

    // Add the new monomer to the current list of monomers
    val complete_external_monomers = replacement_monomers :+ external_monomer.asInstanceOf[MonomerType]

    // Update edges to include the new edge
    val complete_external_edges = Edges :+ new_edge.asInstanceOf[EdgeType]

    // Create the one externally bound graph
    val externally_bound_graph = copy(complete_external_monomers, complete_external_edges)

    // Return
    internally_bound_graphs :+ externally_bound_graph
  }

  def RefineEdgeSiteTowardBoundReturnSite(
    site_to_refine: ModelObservingLocalEdgeSite,
    to_site: ModelAgentSite
  ): (ThisType, ModelReactantEdgeSite) = {
    // Monomer to be refined
    val from_monomer = refinable_monomers.FindOrDie(_.EdgeSites.contains(site_to_refine))

    // Replace the from monomer
    val (replacement_monomer, from_binding_site) = from_monomer.RefineEdgeSiteTowardBound(site_to_refine, to_site)
    val replacement_monomers = {
      Monomers.map{
        case x if x == `from_monomer` => replacement_monomer.asInstanceOf[MonomerType with ModelReactantMonomer]
        case other => other
      }
    }

    // Return
    (copy(replacement_monomers), from_binding_site)
  }

  def RefineEdgeSiteTowardBound(
    site_to_refine: ModelObservingLocalEdgeSite,
    to_site: ModelAgentSite
  ): ThisType = RefineEdgeSiteTowardBoundReturnSite(site_to_refine, to_site)._1

  def RefineEdgeSiteTowardExternalBound(
    site_to_refine: ModelObservingLocalEdgeSite,
    to_site: ModelAgentSite
  ): ThisType = {
    // Monomer to be refined
    val from_monomer = refinable_monomers.FindOrDie(_.EdgeSites.contains(site_to_refine))

    // From site
    val from_site = site_to_refine.AgentSite

    // Replace the from monomer
    val (replacement_monomer, from_binding_site) = from_monomer.RefineEdgeSiteTowardBound(site_to_refine, to_site)
    val replacement_monomers = {
      Monomers.map{
        case x if x == `from_monomer` => replacement_monomer.asInstanceOf[MonomerType with ModelReactantMonomer]
        case other => other
      }
    }

    // Create the one external monomer and edge
    val (external_monomer, new_edge) = {
      site_to_refine match {
        case _: ModelStaticLocalEdgeSite =>
          val to_binding_site = ModelStaticBoundEdgeSite(to_site, ModelPartialEdge(from_site))

          val external_monomer = ModelStaticMonomer(to_site.Parent, EdgeSites = Seq(to_binding_site))

          val new_edge = ModelStaticBoundEdge(Seq(from_binding_site, to_binding_site))

          (external_monomer, new_edge)

        case _: ModelConsumingLocalEdgeSite =>
          val to_binding_site = ModelChangedBoundUnboundEdgeSite(to_site, ModelPartialEdge(from_site))

          val external_monomer = ModelConservedMonomer(to_site.Parent, EdgeSites = Seq(to_binding_site))

          val new_edge = ModelConsumedBoundEdge(Seq(from_binding_site, to_binding_site))

          (external_monomer, new_edge)
      }
    }

    // Add the new monomer to the current list of monomers
    val complete_external_monomers = replacement_monomers :+ external_monomer.asInstanceOf[MonomerType]

    // Update edges to include the new edge
    val complete_external_edges = Edges :+ new_edge.asInstanceOf[EdgeType]

    // Return the externally bound graph
    copy(complete_external_monomers, complete_external_edges)
  }

  def RefineEdgeSiteAgainstBound(
    site_to_refine: ModelObservingLocalEdgeSite,
    to_site: ModelAgentSite
  ): ThisType = {
    // Monomer to be refined
    val from_monomer = refinable_monomers.FindOrDie(_.EdgeSites.contains(site_to_refine))

    // Return
    copy(
      Monomers = Monomers.map{
        case x if x == `from_monomer` =>
          from_monomer.RefineEdgeSiteAgainstBound(site_to_refine, to_site).asInstanceOf[MonomerType]

        case unchanged_monomer =>
          unchanged_monomer
      }
    )
  }

  def RefineCompartmentSiteToward(
    compartment_being_refined: ModelReactantCompartmentSite,
    compartment_refining: ModelStaticCompartmentSite
  ): ThisType = {

    val monomer_to_refine = refinable_monomers.FindOrDie(_.Compartment == compartment_being_refined)

    copy(
      Monomers = Monomers.map{
        case x if x == `monomer_to_refine` =>
          monomer_to_refine.RefineCompartmentToward(compartment_refining).asInstanceOf[MonomerType]

        case unchanged_monomer =>
          unchanged_monomer
      }
    )
  }

  def RefineCompartmentSiteAgainst(
    compartment_being_refined: ModelReactantCompartmentSite,
    compartment_refining: ModelStaticCompartmentSite
  ): ThisType = {

    val monomer_to_refine = refinable_monomers.FindOrDie(_.Compartment == compartment_being_refined)

    this.copy(
      Monomers = Monomers.map{
        case x if x == `monomer_to_refine` =>
          monomer_to_refine.RefineCompartmentAgainst(compartment_refining).asInstanceOf[MonomerType]

        case unchanged_monomer =>
          unchanged_monomer
      }
    )
  }

  def RefineStateSiteToward(
    site_being_refined: ModelReactantStateSite,
    site_refining: ModelStaticStateSite
  ): ThisType = {

    val monomer_to_refine = refinable_monomers.FindOrDie(_.StateSites.contains(site_being_refined))

    copy(
      Monomers = Monomers.map{
        case x if x == `monomer_to_refine` =>
          monomer_to_refine.RefineStateSiteToward(site_being_refined, site_refining).asInstanceOf[MonomerType]

        case unchanged_monomer =>
          unchanged_monomer
      }
    )
  }

  def RefineStateSiteAgainst(
    site_being_refined: ModelReactantStateSite,
    site_refining: ModelStaticStateSite
  ): ThisType = {

    val monomer_to_refine = refinable_monomers.FindOrDie(_.StateSites.contains(site_being_refined))

    copy(
      Monomers = Monomers.map{
        case x if x == `monomer_to_refine` =>
          monomer_to_refine.RefineStateSiteAgainst(site_being_refined, site_refining).asInstanceOf[MonomerType]

        case unchanged_monomer =>
          unchanged_monomer
      }
    )
  }

  def RefineEdgeSiteToward(
    site_being_refined: ModelObservingLocalEdgeSite,
    site_refining: ModelStaticLocalEdgeSite
  ): ThisType = {

    val monomer_to_refine = refinable_monomers.FindOrDie(_.EdgeSites.contains(site_being_refined))

    copy(
      Monomers = Monomers.map{
        case x if x == `monomer_to_refine` =>
          monomer_to_refine.RefineEdgeSiteTowardLocal(site_being_refined, site_refining).asInstanceOf[MonomerType]

        case unchanged_monomer =>
          unchanged_monomer
      }
    )
  }

  def RefineEdgeSiteAgainst(
    site_being_refined: ModelObservingLocalEdgeSite,
    site_refining: ModelStaticLocalEdgeSite
  ): ThisType = {

    val monomer_to_refine = refinable_monomers.FindOrDie(_.EdgeSites.contains(site_being_refined))

    copy(
      Monomers = Monomers.map{
        case x if x == `monomer_to_refine` =>
          monomer_to_refine.RefineEdgeSiteAgainstLocal(site_being_refined, site_refining).asInstanceOf[MonomerType]

        case unchanged_monomer =>
          unchanged_monomer
      }
    )
  }

  // Return a list of partial alignments of a subset of Monomers to a subset of reactant.Monomers where
  //   (1) the first monomer is consumed
  //   (2) the alignment does not disobey
  //   (3) the reactant in this alignment does not fully obey the fragment
  def PartialAlignmentsConsumedBy(reactant: ModelReactantGraph)
    (implicit ev: ThisType <:< ModelStaticGraph):
    Seq[(Seq[ModelStaticMonomer], Seq[ModelReactantMonomer])] = {

    val consumed_monomer_pairs = {
      for {
        fragment_monomer <- ev(this).Monomers
        reactant_monomer <- reactant.Monomers
        if fragment_monomer.IsConsumedBy(reactant_monomer)
      } yield {
        (fragment_monomer, reactant_monomer)
      }
    }

    // Recursive function
    def align(
      fragment_attempt: Seq[ModelStaticMonomer],
      reactant_attempt: Seq[ModelReactantMonomer],
      new_monomer_count: Int
    ): Option[(Seq[ModelStaticMonomer], Seq[ModelReactantMonomer])] = {
      // Find the monomers that need to be processed next
      val range = Range(fragment_attempt.size - new_monomer_count, fragment_attempt.size)
      val new_fragment_monomers = fragment_attempt.slice(range.start, range.end)

      // Find the edges that connect to the new monomers
      val new_fragment_edges =
        ev(this).Edges.filter(edge => new_fragment_monomers.exists(_.BoundEdgeConnectsHere(edge)))

      // Check if edges connected to new monomers do not disobey each other
      var next_fragment_monomers = Seq[ModelStaticMonomer]()
      var next_reactant_monomers = Seq[ModelReactantMonomer]()

      val new_edges_do_not_disobey = {
        new_fragment_edges.forall(
          new_fragment_edge => {
            // Find the objects this fragment edge connects to
            val starting_fragment_monomer =
              new_fragment_monomers.FindOrDie(_.BoundEdgeConnectsHere(new_fragment_edge))

            val starting_fragment_site =
              new_fragment_edge.Targets.FindOrDie(target_site =>
                starting_fragment_monomer.EdgeSites.contains(target_site)
              )

            val starting_indexes =
              (
                fragment_attempt.indexWhere(_ == starting_fragment_monomer),
                starting_fragment_monomer.EdgeSites.indexWhere(_ == starting_fragment_site)
              )

            val ending_fragment_site = new_fragment_edge.Targets.FindOrDie(_ != starting_fragment_site)

            val ending_fragment_monomer = ev(this).MonomerWithEdgeSite(ending_fragment_site)

            // Switch on whether or not the reactant site is a bound edge site too or a local site
            reactant_attempt(starting_indexes._1).EdgeSites(starting_indexes._2) match {
              case reactant_starting_site: ModelObservingLocalEdgeSite =>
                // Local site already determined by monomer check
                true

              case reactant_starting_site: ModelObservingBoundEdgeSite =>
                // Corresponding edge on reactant
                val new_reactant_edge = reactant.Edges.FindOrDie(_.Targets.contains(reactant_starting_site))

                val ending_reactant_site = new_reactant_edge.Targets.FindOrDie(_ != reactant_starting_site)

                val ending_reactant_monomer = reactant.MonomerWithEdgeSite(ending_reactant_site)

                if (fragment_attempt.contains(ending_fragment_monomer)) {
                  // Bound edge on fragment forms cycle
                  val ending_indexes =
                    (
                      fragment_attempt.indexWhere(_ == ending_fragment_monomer),
                      ending_fragment_monomer.EdgeSites.indexWhere(_ == ending_fragment_site)
                    )

                  // Return true if target reactant site is in the same place as the target fragment site
                  ending_reactant_site == reactant_attempt(ending_indexes._1).EdgeSites(ending_indexes._2)
                } else {
                  // Bound edge on fragment goes to a new monomer

                  // Add new monomers to list of next monomers to add
                  next_fragment_monomers :+= ending_fragment_monomer
                  next_reactant_monomers :+= ending_reactant_monomer

                  // Return true if the new monomers do not disobey each other
                  !ending_fragment_monomer.DisobeysContract(ending_reactant_monomer.ToStaticReactant)
                }
            }
          }
        )
      }

/*
      def alignment_fully_obeys(
        fragment_attempt: Seq[ModelStaticMonomer],
        reactant_attempt: Seq[ModelReactantMonomer]
      ): Boolean = {
        val monomers_obey = fragment_attempt.zip(reactant_attempt).forall(
          tuple => tuple._1.ObeysContract(tuple._2.ToStaticReactant)
        )

        // Return
        if (!monomers_obey) {
          false
        } else {
          // Create temporary graph to gain access edge indexing methods
          val fragment_attempt_graph = ModelGraphClass(fragment_attempt, Edges)
          val reactant_attempt_graph = ModelGraphClass(
            reactant_attempt,
            reactant.Edges.collect{case x: ModelReactantBoundEdge => x}
          )

          // Some of these edges may be partially connected
          val fragment_edge_indexes =
            fragment_attempt_graph.Edges.map(_.Targets.flatMap(fragment_attempt_graph.EdgeSiteIndex(_)).toSet)

          // None of these edges can be partially connected. If they obeyed as bound sites, they would have been
          // followed and not been partially connected. If they did not obey, false would already have been returned
          val reactant_edge_indexes = {
            reactant_attempt_graph.Edges
            // Keep only the edges that connect to the contract
            .filter(edge => reactant_attempt_graph.Monomers.exists(_.BoundEdgeConnectsHere(edge)))
            .map(_.Targets.flatMap(reactant_attempt_graph.EdgeSiteIndex(_)).toSet) // FlatMap should see no Nones
          }

          // Every contract edge is also a fragment edge (not necessarily vice versa)
          val edges_obey = reactant_edge_indexes.diff(fragment_edge_indexes).isEmpty

          // Return true is edges obey also
          edges_obey
        }
      }
*/

      // Return: False = None, True and no new monomers = Some if it doesn't fully obey, True and new monomers = recurse
      if (!new_edges_do_not_disobey) {
        None
      } else {
        if (next_fragment_monomers.isEmpty) {
          Some((fragment_attempt, reactant_attempt))
        } else {
          align(
            fragment_attempt ++ next_fragment_monomers,
            reactant_attempt ++ next_reactant_monomers,
            next_fragment_monomers.size
          )
        }
      }
    }

    // Spawn recursive search from each consumed monomer
    val out = consumed_monomer_pairs.view.map(pair => align(Seq(pair._1), Seq(pair._2), 1)).flatten
    out
  }

    /*
     This is a fragment. This function attempts to partially align the fragment and the product while having the overlap
     portion contain a produced member.

     This function accomplishes this by first trying to find a monomer in both the fragment and product where some part
     of the fragment is produced by the product and the two monomers do not disobey each other. Then the bound edges
     are considered recursively. Each bound edge that appears in only the fragment or monomer must not disobey the
     local contract of the other. Each bound edge that appears in both must each connect to monomers that do not
     disobey each other. The connected monomers are added to the alignment and any new bound edges must follow the same
     criteria as before.

     For each iteration, it looks for bound edge sites on all monomers added in the previous iteration. This iteration
     then adds pairs of monomers for the next iteration.
   */
  def PartialAlignmentsProducedBy(product: ModelProductGraph)(implicit ev: ThisType <:< ModelStaticGraph):
    Seq[(Seq[ModelStaticMonomer], Seq[ModelProductMonomer])] = {

    val produced_monomer_pairs = {
      for {
        fragment_monomer <- ev(this).Monomers
        product_monomer <- product.Monomers
        if fragment_monomer.IsProducedBy(product_monomer)
      } yield {
        (fragment_monomer, product_monomer)
      }
    }

    // Recursive function
    def align(
      fragment_attempt: Seq[ModelStaticMonomer],
      product_attempt: Seq[ModelProductMonomer],
      new_monomer_count: Int
    ): Option[(Seq[ModelStaticMonomer], Seq[ModelProductMonomer])] = {
      // Find the monomers that need to be processed next
      val range = Range(fragment_attempt.size - new_monomer_count, fragment_attempt.size)
      val new_fragment_monomers = fragment_attempt.slice(range.start, range.end)

      // Find the edges that connect to the new monomers
      val new_fragment_edges =
        ev(this).Edges.filter(edge => new_fragment_monomers.exists(_.BoundEdgeConnectsHere(edge)))

      // Check if edges connected to new monomers do not disobey each other
      var next_fragment_monomers = Seq[ModelStaticMonomer]()
      var next_product_monomers = Seq[ModelProductMonomer]()

      val new_edges_do_not_disobey = {
        new_fragment_edges.forall(
          new_fragment_edge => {
            // Find the objects this fragment edge connects to
            val starting_fragment_monomer =
              new_fragment_monomers.FindOrDie(_.BoundEdgeConnectsHere(new_fragment_edge))

            val starting_fragment_site =
              new_fragment_edge.Targets.FindOrDie(target_site =>
                starting_fragment_monomer.EdgeSites.contains(target_site)
              )

            val starting_indexes =
              (
                fragment_attempt.indexOf(starting_fragment_monomer),
                starting_fragment_monomer.EdgeSites.indexOf(starting_fragment_site)
              )

            val ending_fragment_site = new_fragment_edge.Targets.FindOrDie(_ != starting_fragment_site)

            val ending_fragment_monomer = ev(this).MonomerWithEdgeSite(ending_fragment_site)

            // Switch on whether or not the product site is a bound edge site too or a local site
            product_attempt(starting_indexes._1).EdgeSites(starting_indexes._2) match {
              case product_starting_site: ModelResultingLocalEdgeSite =>
                // Local site already determined by monomer check
                true

              case product_starting_site: ModelResultingBoundEdgeSite =>
                // Corresponding edge on reactant
                val new_reactant_edge = product.Edges.FindOrDie(_.Targets.contains(product_starting_site))

                val ending_product_site = new_reactant_edge.Targets.FindOrDie(_ != product_starting_site)

                val ending_product_monomer = product.MonomerWithEdgeSite(ending_product_site)

                if (fragment_attempt.contains(ending_fragment_monomer)) {
                  // Bound edge on fragment forms cycle
                  val ending_indexes =
                    (
                      fragment_attempt.indexWhere(_ == ending_fragment_monomer),
                      ending_fragment_monomer.EdgeSites.indexWhere(_ == ending_fragment_site)
                    )

                  // Return true if target reactant site is in the same place as the target fragment site
                  ending_product_site == product_attempt(ending_indexes._1).EdgeSites(ending_indexes._2)
                } else {
                  // Bound edge on fragment goes to a new monomer

                  // Add new monomers to list of next monomers to add
                  next_fragment_monomers :+= ending_fragment_monomer
                  next_product_monomers :+= ending_product_monomer

                  // Return true if the new monomers do not disobey each other
                  !ending_fragment_monomer.DisobeysContract(ending_product_monomer.ToStaticProduct)
                }
            }
          }
        )
      }

/*
      def alignment_fully_obeys(
        product_attempt: Seq[ModelProductMonomer],
        fragment_attempt: Seq[ModelStaticMonomer]
      ): Boolean = {
        val monomers_obey = product_attempt.zip(fragment_attempt).forall(
          tuple => tuple._1.ToStaticProduct.ObeysContract(tuple._2)
        )

        // Return
        if (!monomers_obey) {
          false
        } else {
          // Create temporary graph to gain access edge indexing methods
          val product_attempt_graph = ModelGraphClass(
            product_attempt,
            product.Edges.collect{case x: ModelProductBoundEdge => x}
          )
          val fragment_attempt_graph = ModelGraphClass(fragment_attempt, ev(this).Edges)

          // Some of these edges may be partially connected
          val product_edge_indexes =
            product_attempt_graph.Edges.map(_.Targets.flatMap(product_attempt_graph.EdgeSiteIndex(_)).toSet)

          // None of these edges can be partially connected. If they obeyed as bound sites, they would have been
          // followed and not been partially connected. If they did not obey, false would already have been returned
          val fragment_edge_indexes = {
            fragment_attempt_graph.Edges
            // Keep only the edges that connect to the contract
            .filter(edge => fragment_attempt_graph.Monomers.exists(_.BoundEdgeConnectsHere(edge)))
            .map(_.Targets.flatMap(fragment_attempt_graph.EdgeSiteIndex(_)).toSet) // FlatMap should see no Nones
          }

          // Every contract edge is also a fragment edge (not necessarily vice versa)
          val edges_obey = fragment_edge_indexes.diff(product_edge_indexes).isEmpty

          // Return true is edges obey also
          edges_obey
        }
      }
*/

      // Return: False = None, True and no new monomers = Some if it doesn't fully obey, True and new monomers = recurse
      if (!new_edges_do_not_disobey) {
        None
      } else {
        if (next_fragment_monomers.isEmpty) {
          Some((fragment_attempt, product_attempt))
        } else {
          align(
            fragment_attempt ++ next_fragment_monomers,
            product_attempt ++ next_product_monomers,
            next_fragment_monomers.size
          )
        }
      }
    }

    // Spawn recursive search from each consumed monomer
    val all_alignments = produced_monomer_pairs.view.map(pair => align(Seq(pair._1), Seq(pair._2), 1)).flatten

    // Return unique alignments
    // If multiple monomers are produced, then this may result in identical alignments
    all_alignments.map(tuple => tuple.zipped.toSet)
    .distinct
    .map(zipped => zipped.toSeq.unzip)
  }

  // The input graphs are allowed to have bound edge sites that do not have associated bound edges
  private def alignment_fully_obeys(
    alignment_attempt: ModelStaticGraph,
    contract_attempt: ModelStaticGraph
  ): Boolean = {
    val monomers_obey = alignment_attempt.Monomers.zip(contract_attempt.Monomers).forall(
      tuple => tuple._1.ObeysContract(tuple._2)
    )

    // Return
    if (!monomers_obey) {
      false
    } else {
      val alignment_edge_indexes =
        alignment_attempt.Edges.map(_.Targets.flatMap(alignment_attempt.EdgeSiteIndex(_)).toSet) // FlatMap should see no Nones

      val contract_edge_indexes = {
        contract_attempt.Edges
        // Keep only the edges that connect to the contract
        .filter(edge => contract_attempt.Monomers.exists(_.BoundEdgeConnectsHere(edge)))
        .map(_.Targets.flatMap(contract_attempt.EdgeSiteIndex(_)).toSet) // FlatMap should see no Nones
      }

      // Every contract edge is also an alignment edge (return true if vice versa is true too)
      val edges_obey = contract_edge_indexes.diff(alignment_edge_indexes).isEmpty

      // Return true if edges obey also
      edges_obey
    }
  }

  // If a local site consumes partial edges, then they partially consume the partial edges on the other side. It seems
  // that the easiest way to deal with this is to create an alignment monomer that emulates a bound edge being broken
  def PartialAlignmentsForConsumedPartialEdges(fragment: ModelStaticGraph)
    (implicit ev: ThisType <:< ModelReactantGraph):
    Seq[(ModelConsumingLocalEdgeSite, ModelStaticLocalEdgeSite)] = {

    for {
      // Reactant sites must consume partial edges in order to induce the consumption of partial edges in the fragment
      reactant_site <- Monomers.flatMap(_.EdgeSites).collect{case x: ModelConsumingLocalEdgeSite => x}

      // Fragment site must have partial edges to be consumed
      fragment_site <- fragment.Monomers.flatMap(_.EdgeSites).collect{case x: ModelStaticLocalEdgeSite => x}

      // Fragment site must not contain an unbound in order to be consumed
      if !fragment_site.Static.contains(ModelUnboundEdge)

      // Reactant site must break a partial edge with the fragment site
      if reactant_site.Observed.contains(ModelPartialEdge(fragment_site.AgentSite))

      // Fragment site must start out with a partial edge with the reactant site
      if fragment_site.Static.contains(ModelPartialEdge(reactant_site.AgentSite))
    } yield {
      (reactant_site, fragment_site)
    }
  }

  def ProductSitesPartiallyProducingFragmentViaPartialEdge(fragment: ModelStaticGraph):
    Seq[(ModelConsumingLocalEdgeSite, ModelStaticLocalEdgeSite)] = {

    for {
      // Product site must consume partial edges in order to induce the production of unbounds in the fragment
      product_site <- Monomers.flatMap(_.EdgeSites).collect{case x: ModelConsumingLocalEdgeSite => x}

      // Fragment site must have partial edges to be consumed
      fragment_site <- fragment.Monomers.flatMap(_.EdgeSites).collect{case x: ModelStaticLocalEdgeSite => x}

      // Fragment site must contain an unbound edge (which is produced)
      if fragment_site.Static.contains(ModelUnboundEdge)

      // Product site must actually break a partial edge with the fragment site
      if product_site.Observed.contains(ModelPartialEdge(fragment_site.AgentSite))

      // Fragment site must not start out with a partial edge with the product site
      if !fragment_site.Static.contains(ModelPartialEdge(product_site.AgentSite))
    } yield {
      (product_site, fragment_site)
    }
  }

  def RefineConsumedAroundReactant(reactant: ModelReactantGraph)
    (implicit ev: ThisType <:< ModelStaticGraph): Seq[ThisType] = {

    // Keep alignments that are strictly partial
    val portions_that_need_refined = {
      ev(this).PartialAlignmentsConsumedBy(reactant)
      .filter(tuple => {
        val aligned_fragment = ModelGraphClass(tuple._1, ev(this).Edges)
        val aligned_static_reactant = ModelGraphClass(tuple._2, reactant.Edges).remove_unlinked_edges.ToStaticReactant

        // Return
        !alignment_fully_obeys(aligned_fragment, aligned_static_reactant)
      })
    }

    val sites_to_refine_for_partial_edge_consumption = reactant.PartialAlignmentsForConsumedPartialEdges(this)

    if (portions_that_need_refined.isEmpty && sites_to_refine_for_partial_edge_consumption.isEmpty) {
      // If fragment already matches fully, then do no more refinement
      Seq(this)
    } else if (portions_that_need_refined.isEmpty) {
      val sites_to_refine = sites_to_refine_for_partial_edge_consumption.head

      // The site that the rule disconnects that needs to be mentioned explicitly
      val to_site = sites_to_refine._1.AgentSite

      val toward_graphs = RefineEdgeSiteTowardBoundMonomer(sites_to_refine._2, to_site)
      val against_graph = RefineEdgeSiteAgainstBound(sites_to_refine._2, to_site)

      val new_graphs = {
        if(against_graph.ToStaticReactant.GraphCanExist) {
          toward_graphs :+ against_graph
        } else {
          toward_graphs
        }
      }

      // Return
      new_graphs.map(_.RefineConsumedAroundReactant(reactant)).flatten
    } else {
      val (fragment_monomers, reactant_monomers) = portions_that_need_refined.head

      // Look for local sites that need refining
      // (Keep only the first monomer since this can only be done one at a time. Plus, since the monomers were
      // aligned, this will select sites closest to the consuming part first.)
      val option_sites_to_refine = {
        fragment_monomers.zip(reactant_monomers)
        .map(tuple =>
          tuple._1.ReactantNeedsLocalRefinementForContract(tuple._2.ToStaticReactant)
        )
        .find(_ != (None, Nil, Nil))
      }

      val new_graphs: Seq[ThisType] = option_sites_to_refine match {
        // Did the compartment need refined?
        case Some((Some(compartment_to_refine), _, _)) =>
          val toward_graph = RefineCompartmentSiteToward(compartment_to_refine._1, compartment_to_refine._2)
          val against_graph = RefineCompartmentSiteAgainst(compartment_to_refine._1, compartment_to_refine._2)

          Seq(toward_graph, against_graph)

        // Did a state site need refined?
        case Some((_, Seq(state_site_to_refine, _*), _)) =>
          val toward_graph = RefineStateSiteToward(state_site_to_refine._1, state_site_to_refine._2)
          val against_graph = RefineStateSiteAgainst(state_site_to_refine._1, state_site_to_refine._2)

          Seq(toward_graph, against_graph)

        // Did an edge need *local* refinement? This won't deal with bound edges that need refined
        case Some((_, _, Seq(edge_site_to_refine, _*))) =>
          val toward_graph = RefineEdgeSiteToward(edge_site_to_refine._1, edge_site_to_refine._2)
          val against_graph = RefineEdgeSiteAgainst(edge_site_to_refine._1, edge_site_to_refine._2)

          Seq(toward_graph, against_graph)

        // No local refinement was needed, so adjacent refinement must be needed
        case None =>
          // Find the sites that are bound in the reactant, but not bound in the fragment
          val sites_to_refine = {
            fragment_monomers.zip(reactant_monomers)
            .map(tuple =>
              tuple._1.FragmentNeedsAdjacentRefinementForReactant(tuple._2)
            )
            .FindOrDie(_ != Nil)
            .head
          }

          // The agent site that the fragment connects to
          val to_site = sites_to_refine._2.Observed(0).Site

          // Do refinement
          val toward_graphs = RefineEdgeSiteTowardBoundMonomer(sites_to_refine._1, to_site)
          val against_graph = RefineEdgeSiteAgainstBound(sites_to_refine._1, to_site)

          // Return
          if(against_graph.ToStaticReactant.GraphCanExist) {
            toward_graphs :+ against_graph
          } else {
            toward_graphs
          }
      }

      // Return
      new_graphs.map(_.RefineConsumedAroundReactant(reactant)).flatten
    }
  }

  def AllProducingAlignmentsRefinedTowardFragment(fragment: ModelStaticGraph): Seq[ThisType] = {
    // Find all normal producing alignments
    val partial_alignments = fragment.PartialAlignmentsProducedBy(ToProduct)

    // Create a new product graph for each alignment
    val new_normal_graphs = {
      for ((fragment_monomers, product_monomers) <- partial_alignments) yield {

        // Look for local sites that need refining. Only static sites will come out of this because only static sites
        // can can have properties that neither obey nor disobey a fragment in their resulting side.
        val (refinable_product_monomers, refinable_fragment_monomers) = {
          product_monomers.zip(fragment_monomers)
          // Only conserved monomers can be refined
          .collect{case (x, y) if x.IsModelReactantMonomer => (x.asInstanceOf[ModelReactantMonomer], y)}
          .unzip
        }

        // Fragment monomers that need to be added
        val fragment_monomers_to_add = fragment.Monomers.filterNot(fragment_monomers.contains(_))

        // Product monomers that need to be added
        val product_monomers_to_add = Monomers.filterNot(product_monomers.contains(_))

        // Make a fragment graph that is aligned with the product
        val realigned_fragment_graph = ModelGraphClass(fragment_monomers ++ fragment_monomers_to_add, fragment.Edges)

        // Make a product graph that is aligned with the fragment. Repeatedly refine this graph toward the contract
        var new_graph = ModelGraphClass[MonomerType, EdgeType](
          // DeepCopy the fragment monomers because they could refine this rule again and end up as multiples
          (product_monomers ++ fragment_monomers_to_add.map(_.DeepCopy()) ++ product_monomers_to_add).asInstanceOf[Seq[MonomerType]],
          Edges
        )

        // All local sites that need refinement
        val local_sites_to_refine = {
          refinable_product_monomers.zip(refinable_fragment_monomers)
          .map(tuple =>
            tuple._1.ProductNeedsLocalRefinementForContract(tuple._2)
          )
        }

        // Loop over each monomer, refining all the sites
        for (sites <- local_sites_to_refine) {
          // Refine compartment
          for ((product_compartment, fragment_compartment) <- sites._1) {
            new_graph = new_graph.RefineCompartmentSiteToward(product_compartment, fragment_compartment)
          }

          // Refine state sites
          for ((product_state_site, fragment_state_site) <- sites._2) {
            new_graph = new_graph.RefineStateSiteToward(product_state_site, fragment_state_site)
          }

          // Refine local edge sites
          for ((product_edge_site, fragment_edge_site) <- sites._3) {
            new_graph = new_graph.RefineEdgeSiteToward(product_edge_site, fragment_edge_site)
          }
        }

        // All bound edge sites that need refinement
        val bound_edge_sites_to_refine = {
          refinable_product_monomers.zip(refinable_fragment_monomers)
          .map(tuple =>
            tuple._1.ProductNeedsAdjacentRefinementForFragment(tuple._2)
          )
          .flatten
        }

        // Refine bound edges toward a bound edge site
        for ((product_local_site, fragment_bound_site) <- bound_edge_sites_to_refine) {
          new_graph = new_graph.RefineEdgeSiteTowardBound(product_local_site, fragment_bound_site.Static.Site)
        }

        // Indexes for current product edges
        val product_edge_indexes =
          new_graph.Edges.map(_.Targets.flatMap(new_graph.EdgeSiteIndex(_)).toSet)

        // Loop over every edge in the fragment. Ensure that the product has each of these
        for (edge <- fragment.Edges) {
          val fragment_edge_index = edge.Targets.flatMap(realigned_fragment_graph.EdgeSiteIndex(_)).toSet

          if (!product_edge_indexes.contains(fragment_edge_index)) {
            val new_edge = ModelStaticBoundEdge(
              fragment_edge_index.toSeq.map(tuple =>
                new_graph.Monomers(tuple._1).EdgeSites(tuple._2)
              )
            )

            new_graph = ModelGraphClass(
              new_graph.Monomers,
              new_graph.Edges :+ new_edge.asInstanceOf[EdgeType]
            )
          }
        }

        // Return
        new_graph
      }
    }

    // Find all producing alignments from partial edge conversion to unbound
    //#TODO: Consider what happens when more than one partial edge is consumed at once or the fragment overlaps with the product
    val sites_to_refine_for_partial_edge_consumption = ProductSitesPartiallyProducingFragmentViaPartialEdge(fragment)

    val new_partial_graphs = {
      for ((product_site, fragment_site) <- sites_to_refine_for_partial_edge_consumption) yield {
        // Make a product graph that is the combination of the product and fragment
        var new_graph = ModelGraphClass[MonomerType, EdgeType](
          (Monomers ++ fragment.Monomers).asInstanceOf[Seq[MonomerType]],
          (Edges ++ fragment.Edges).asInstanceOf[Seq[EdgeType]]
        )

        // Refine sites to bound; keep the identities of the sites
        val tuple1 = new_graph.RefineEdgeSiteTowardBoundReturnSite(product_site, fragment_site.AgentSite)
        new_graph = tuple1._1
        val product_side_site = tuple1._2

        val tuple2 = new_graph.RefineEdgeSiteTowardBoundReturnSite(fragment_site, product_site.AgentSite)
        new_graph = tuple2._1
        val fragment_site_site = tuple2._2

        // Build an edge to connect the sites
        val new_edge = ModelConsumedBoundEdge(Seq(product_side_site, fragment_site_site))
        new_graph = ModelGraphClass(new_graph.Monomers, new_graph.Edges :+ new_edge.asInstanceOf[EdgeType])

        // Return
        new_graph
      }
    }

    // Combine all new graphs
    new_normal_graphs ++ new_partial_graphs
  }

/*
  def RefineProducingAroundFragment(fragment: ModelStaticGraph): Seq[ThisType] = {

    // Keep only alignments that do not fully obey
    val portions_that_need_refined = {
      fragment.PartialAlignmentsProducedBy(ToProduct)
      .filter(tuple => {
        val aligned_static_product = ModelGraphClass(tuple._2, this.Edges).remove_unlinked_edges.ToStaticProduct
        val aligned_fragment = ModelGraphClass(tuple._1, fragment.Edges)

        // Return
        !alignment_fully_obeys(aligned_static_product, aligned_fragment)
      })
    }

    val sites_to_refine_for_partial_edge_consumption = ProductSitesPartiallyProducingFragmentViaPartialEdge(fragment)

    if (portions_that_need_refined.isEmpty && sites_to_refine_for_partial_edge_consumption.isEmpty) {
      // If product has no partial alignments, do no refinements
      Seq(this)
    } else if (portions_that_need_refined.isEmpty) {
      val sites_to_refine = sites_to_refine_for_partial_edge_consumption.head

      // The site that the rule disconnects that needs to be mentioned explicitly
      val to_site = sites_to_refine._2.AgentSite

      val toward_graphs = RefineEdgeSiteTowardBoundMonomer(sites_to_refine._1, to_site)
      val against_graph = RefineEdgeSiteAgainstBound(sites_to_refine._1, to_site)

      val new_graphs = {
        if(against_graph.ToStaticReactant.GraphCanExist) {
          toward_graphs :+ against_graph
        } else {
          toward_graphs
        }
      }

      // Return
      new_graphs.map(_.RefineProducingAroundFragment(fragment)).flatten
    } else {
      val (fragment_monomers, product_monomers) = portions_that_need_refined.head

      // Look for local sites that need refining. Only static sites will come out of this because only static sites
      // can can have properties that neither obey nor disobey a fragment in their resulting side.
      // (Keep only the first monomer since this can only be done one at a time. Plus, since the monomers were
      // aligned, this will select sites closest to the producing part first.)
      val (refinable_product_monomers, refinable_fragment_monomers) = {
        product_monomers.zip(fragment_monomers)
        // Only conserved monomers can be refined
        .collect{case (x, y) if x.IsModelReactantMonomer => (x.asInstanceOf[ModelReactantMonomer], y)}
        .unzip
      }

      val option_sites_to_refine = {
        refinable_product_monomers.zip(refinable_fragment_monomers)
        .map(tuple =>
          tuple._1.ProductNeedsLocalRefinementForContract(tuple._2)
        )
        .find(_ != (None, Nil, Nil))
      }

      val new_graphs = option_sites_to_refine match {
        // Did the compartment need refined?
        case Some((Some(compartment_to_refine), _, _)) =>
          val toward_graph = RefineCompartmentSiteToward(compartment_to_refine._1, compartment_to_refine._2)
          val against_graph = RefineCompartmentSiteAgainst(compartment_to_refine._1, compartment_to_refine._2)

          Seq(toward_graph, against_graph)

        // Did a state site need refined?
        case Some((_, Seq(state_site_to_refine, _*), _)) =>
          val toward_graph = RefineStateSiteToward(state_site_to_refine._1, state_site_to_refine._2)
          val against_graph = RefineStateSiteAgainst(state_site_to_refine._1, state_site_to_refine._2)

          Seq(toward_graph, against_graph)

        // Did an edge need *local* refinement? This won't deal with bound edges that need refined
        case Some((_, _, Seq(edge_site_to_refine, _*))) =>
          val toward_graph = RefineEdgeSiteToward(edge_site_to_refine._1, edge_site_to_refine._2)
          val against_graph = RefineEdgeSiteAgainst(edge_site_to_refine._1, edge_site_to_refine._2)

          Seq(toward_graph, against_graph)

        // No local refinement was needed, so adjacent refinement must be needed
        case None =>
          // Find the sites that are bound in the reactant, but not bound in the fragment
          val sites_to_refine = {
            refinable_product_monomers.zip(refinable_fragment_monomers)
            .map(tuple =>
              tuple._1.ProductNeedsAdjacentRefinementForFragment(tuple._2)
            )
            .FindOrDie(_ != Nil)
            .head
          }

          // The agent site that the fragment connects to and the product should connect to
          val to_site = sites_to_refine._2.Static.Site

          // Do refinement
          val toward_graphs = RefineEdgeSiteTowardBoundMonomer(sites_to_refine._1, to_site)
          val against_graph = RefineEdgeSiteAgainstBound(sites_to_refine._1, to_site)

          // Return
          if(against_graph.ToStaticReactant.GraphCanExist) {
            toward_graphs :+ against_graph
          } else {
            toward_graphs
          }
      }

      // Return
      new_graphs.map(_.RefineProducingAroundFragment(fragment)).flatten
    }
  }
*/

  // For this alignment of this.Monomers, refine around the given fragment and return the refined graphs
  def RefineFullAlignmentsAroundFragment(fragment: ModelStaticGraph)
    (implicit ev: ThisType <:< ModelStaticGraph): Seq[ModelStaticGraph] = {

      // Look for local sites that need refining
      // (Keep only the first monomer since this can only be done one at a time.)
      val option_sites_to_refine = {
        ev(this).Monomers.zip(fragment.Monomers)
        .map(tuple =>
          tuple._1.ReactantNeedsLocalRefinementForContract(tuple._2.ToStaticReactant)
        )
        .find(_ != (None, Nil, Nil))
      }

      val new_graphs: Seq[ModelStaticGraph] = option_sites_to_refine match {
        // Did the compartment need refined?
        case Some((Some(compartment_to_refine), _, _)) =>
          val toward_graph = RefineCompartmentSiteToward(compartment_to_refine._1, compartment_to_refine._2)
          val against_graph = RefineCompartmentSiteAgainst(compartment_to_refine._1, compartment_to_refine._2)

          (toward_graph.RefineFullAlignmentsAroundFragment(fragment) :+ against_graph.asInstanceOf[ModelStaticGraph])
          //Seq(toward_graph, against_graph)

        // Did a state site need refined?
        case Some((_, Seq(state_site_to_refine, _*), _)) =>
          val toward_graph = RefineStateSiteToward(state_site_to_refine._1, state_site_to_refine._2)
          val against_graph = RefineStateSiteAgainst(state_site_to_refine._1, state_site_to_refine._2)

          (toward_graph.RefineFullAlignmentsAroundFragment(fragment) :+ against_graph.asInstanceOf[ModelStaticGraph])
          //Seq(toward_graph, against_graph)

        // Did an edge need *local* refinement? This won't deal with bound edges that need refined
        case Some((_, _, Seq(edge_site_to_refine, _*))) =>
          val toward_graph = RefineEdgeSiteToward(edge_site_to_refine._1, edge_site_to_refine._2)
          val against_graph = RefineEdgeSiteAgainst(edge_site_to_refine._1, edge_site_to_refine._2)

          (toward_graph.RefineFullAlignmentsAroundFragment(fragment) :+ against_graph.asInstanceOf[ModelStaticGraph])
          //Seq(toward_graph, against_graph)

        // No local refinement was needed, add a monomer
        case None =>
          if (this.Monomers.size == fragment.Monomers.size) {
            // No more refinement can be made
            Seq(this)
          } else {
            // Next monomer in fragment that needs to be matched
            val previous_monomers = fragment.Monomers.slice(0, this.Monomers.size)
            val monomer_to_be_matched = fragment.Monomers(this.Monomers.size) // already has +1 for 0-indexing

            // Find an edge that connects to the monomer to be matched and a previous monomer
            val refining_edge = fragment.Edges.FindOrDie(
              edge => !edge.Targets.intersect(monomer_to_be_matched.EdgeSites).isEmpty
                && !edge.Targets.intersect(previous_monomers.flatMap(_.EdgeSites)).isEmpty
            )

            // The refining sites
            val remote_refining_site = refining_edge.Targets.intersect(monomer_to_be_matched.EdgeSites).head
            val local_refining_site = refining_edge.Targets.diff(Seq(remote_refining_site)).head

            // Indexes
            val (monomer_index, edge_site_index) = {
              (for {
                i_monomer <- 0 until previous_monomers.size
                i_edge_site <- 0 until previous_monomers(i_monomer).EdgeSites.size
                if fragment.Monomers(i_monomer).EdgeSites(i_edge_site) == local_refining_site
              } yield {
                (i_monomer, i_edge_site)
              })
              .head // There will be exactly one
            }

            // The site to be refined
            val local_needs_refined_site = this.Monomers(monomer_index).EdgeSites(edge_site_index).asInstanceOf[ModelObservingLocalEdgeSite]

            // The agent site that this fragment will connect to
            val to_site = remote_refining_site.AgentSite

            // Do refinement
            val toward_graphs = RefineEdgeSiteTowardBoundMonomer(local_needs_refined_site, to_site)
            val against_graph = RefineEdgeSiteAgainstBound(local_needs_refined_site, to_site)

            // Return
            if(against_graph.ToStaticReactant.GraphCanExist) {
              (toward_graphs.flatMap(_.RefineFullAlignmentsAroundFragment(fragment)) :+ against_graph.asInstanceOf[ModelStaticGraph])
              //(toward_graphs :+ against_graph).asInstanceOf[Seq[ModelStaticGraph]]
            } else {
              toward_graphs.flatMap(_.RefineFullAlignmentsAroundFragment(fragment))
              //toward_graphs.asInstanceOf[Seq[ModelStaticGraph]]
            }
          }
      }

      // Return
      new_graphs
  }

  override def toString = {
    val static_this = {
      this match {
        case x if x.IsStaticGraph => x.asInstanceOf[ModelStaticGraph]
        case x if x.IsReactantGraph => x.asInstanceOf[ModelReactantGraph].ToStaticReactant
        case x if x.IsProductGraph => x.asInstanceOf[ModelProductGraph].ToStaticProduct
        case _ => this.ToStaticReactant
      }
    }

    val site_to_number_map = {
      // Edges and a number
      static_this.Edges
      .zip(1.to(static_this.Edges.size).map(_.toString))
      .map(tuple =>
        tuple._1.Targets.map(target =>
          (target, tuple._2)
        )
      )
      .flatten
      .toMap
    }

    static_this.Monomers.map(_.ToStringWithEdgeNumbers(site_to_number_map)).mkString(".")
  }
}

object ModelCoreGraph {
  def apply(monomers: Seq[ModelMonomer], edges: Seq[ModelBoundEdge]) =
    ModelGraphClass(monomers, edges)
}

object ModelReactantGraph {
  def apply(monomers: Seq[ModelReactantMonomer], edges: Seq[ModelReactantBoundEdge]) =
    ModelGraphClass(monomers, edges)

  def apply(
    original: RuleGraph,
    compartments: Seq[ModelCompartment],
    agents: Seq[ModelAgent],
    edge_map: Map[Int,Buffer[(ModelEdgeSite,Int,Int)]]
  ): ModelReactantGraph = {
    val monomers =
      for (old_monomer <- original.Monomers) yield {
        // Switch on conserved versus consumed monomers
        if (old_monomer.Index != Some(0)) {
          ModelConservedMonomer(old_monomer, compartments, agents, edge_map)
        } else {
          ModelConsumedMonomer(old_monomer, compartments, agents, edge_map)
        }
      }

    // Every list should have exactly two sites by now; convert to tuple
    val edge_tuples = {
      edge_map.values
      .filter(int => original.BoundEdgeIndexes.contains(int))
      .map(seq => (seq(0), seq(1)))
    }

    // Convert all edges to observed edges
    val edges = {
      edge_tuples.map(
        tuple =>
          ModelStaticBoundEdge(Seq(tuple._1._1, tuple._2._1))
      )
      .toSeq
    }

    // Return (edges are applied at the rule level)
    val result = ModelReactantGraph(monomers, edges)
    result
  }
}

object ModelProductGraph {
  def apply(monomers: Seq[ModelProductMonomer], edges: Seq[ModelProductBoundEdge]) =
    ModelGraphClass(monomers, edges)

  def apply(
    original: RuleGraph,
    compartments: Seq[ModelCompartment],
    agents: Seq[ModelAgent],
    edge_map: Map[Int,Buffer[(ModelEdgeSite,Int,Int)]]
  ): ModelProductGraph = {
    val monomers =
      for (old_monomer <- original.Monomers) yield {
        // Switch on conserved versus produced monomers
        if (old_monomer.Index != Some(0)) {
          ModelConservedMonomer(old_monomer, compartments, agents, edge_map)
        } else {
          ModelProducedMonomer(old_monomer, compartments, agents, edge_map)
        }
      }

    // Every list should have exactly two sites by now; convert to tuple
    val edge_tuples = {
      edge_map.values
      .filter(int => original.BoundEdgeIndexes.contains(int))
      .map(seq => (seq(0), seq(1)))
    }

    // Convert all edges to observed edges
    val edges = {
      edge_tuples.map(
        tuple =>
          ModelStaticBoundEdge(Seq(tuple._1._1, tuple._2._1))
      )
      .toSeq
    }

    // Return (edges are applied at the rule level)
    ModelProductGraph(monomers, edges)
  }
}

object ModelStaticGraph {
  def apply(monomers: Seq[ModelStaticMonomer], edges: Seq[ModelStaticBoundEdge]) =
    ModelGraphClass(monomers, edges)

  def apply(original: Graph, compartments: Seq[ModelCompartment], agents: Seq[ModelAgent]): ModelStaticGraph = {
    // Initialize collection for holding edges to be processed
    val edge_map: Map[Int,Buffer[ModelEdgeSite]] = init_observable_edge_map(original.BoundEdgeIndexes)

    // Convert all monomers
    val monomers = original.Monomers.map(monomer => ModelStaticMonomer(monomer, compartments, agents, edge_map))

    // Convert all edges to observed edges
    val edges = edge_map.values.map(value => ModelStaticBoundEdge(value.toSeq)).toSeq

    // Return
    ModelStaticGraph(monomers, edges).BoundEdgeSitesRebuilt()
  }

  def init_observable_edge_map[A](list: Seq[Int]): Map[Int, Buffer[A]] =
    Map() ++ list.map(int => (int, ArrayBuffer[A]()))
}
