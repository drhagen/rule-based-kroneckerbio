/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

import collection.mutable.{Buffer, ArrayBuffer}
import kroneckerbio.model._
import kroneckerbio.model.refining.refining._
import kroneckerbio.utilities.RichAny._

case class ModelRule(
  Name: Option[String],
  Reactants: Seq[ModelRulePattern],
  ProducedMonomers: Seq[ModelProducedMonomer],
  ProducedEdges: Seq[ModelProducedBoundEdge],
  Parameter: ModelRuleParameter
) {
  //#TODO: make copy relink edges when new reactants are added

  val Core = {
    val all_monomers = Reactants.flatMap(_.Main.Monomers) ++ ProducedMonomers

    val all_edges = Reactants.flatMap(_.Main.Edges) ++ ProducedEdges

    // Return
    ModelCoreGraph(all_monomers, all_edges)
  }

  val Products: Seq[ModelProductGraph] = Core.ToProduct.SplitIntoConnected

  def BoundEdgeSitesRebuilt: ModelRule = {
    val reactant_insertion_map = Core.ReactantEdgeTargetTypeMap

    val produced_insertion_map = Core.ProducedEdgeTargetTypeMap

    // Rebuild reactants
    val new_reactants = {
      Reactants.map(reactant =>
        reactant.copy(
          Main = reactant.Main.BoundEdgeSitesRebuilt(reactant_insertion_map, produced_insertion_map)
        )
      )
    }

    // Rebuild produced monomers
    val new_produced_monomers =
      ProducedMonomers.map(_.BoundEdgeSitesRebuilt(reactant_insertion_map, produced_insertion_map))

    // Align old sites with new sites to create a map for the edges
    val original_sites =
      Reactants.flatMap(_.Main.Monomers).flatMap(_.EdgeSites) ++ ProducedMonomers.flatMap(_.EdgeSites)

    val new_sites =
      new_reactants.flatMap(_.Main.Monomers).flatMap(_.EdgeSites) ++ new_produced_monomers.flatMap(_.EdgeSites)

    val edge_site_map = original_sites.zip(new_sites).toMap

    // Remap the produced edges (reactant edges were remapped with the reactants)
    val new_produced_edges = ProducedEdges.map(edge =>
      ModelProducedBoundEdge(edge.Targets.map(edge_site_map(_)))
    )

    // Return
    copy(
      Reactants = new_reactants,
      ProducedMonomers = new_produced_monomers,
      ProducedEdges = new_produced_edges
    )
  }

  //////
  // Methods for agent reduction
  //////
  def AllProducedMonomers: Seq[ModelAgent] =
    Core.AllProducedMonomerNames

  def AllProducedMonomerCompartmentNames: Seq[(ModelAgent,ModelCompartment)] =
    Core.AllProducedMonomerCompartmentNames

  def AllProducedMonomerSiteStateNames: Seq[(ModelAgent,ModelAgentSite,ModelAgentState)] =
    Core.AllProducedMonomerSiteStateNames

  def AllProducedBoundEdgeSitePairNames: Seq[Set[(ModelAgent,ModelAgentSite)]] =
    Core.AllProducedBoundEdgeSitePairNames

  def Reduced(compartments: Seq[ModelCompartment]) = {
    val edge_site_tuples = Buffer[(ModelEdgeSite,ModelEdgeSite)]()

    val reactants = Reactants.map(_.Reduced(edge_site_tuples))

    val produced_monomers = ProducedMonomers.map(_.Reduced(edge_site_tuples))

    val edge_site_map = edge_site_tuples.toMap.withDefault(identity(_))

    val produced_edges = {
      ProducedEdges.map(edge =>
        ModelProducedBoundEdge(edge.Targets.map(edge_site_map(_)))
      )
    }

    // Return
    copy(Reactants = reactants, ProducedMonomers = produced_monomers, ProducedEdges = produced_edges)
  }

  /**
   * Rules must be unimolecular and completely ambiguous in rule compartment, or they can be unambiguous
   */
  def RuleCompartmentsRefined(compartments: Seq[ModelCompartment]): Seq[ModelRule] = {
    Reactants.size match {
      // Case 0: Can only have one compartment anyway, do nothing
      case 0 => Seq(this)

      // Case 1: If compartments are completely ambiguous, do nothing; otherwise, expand
      case 1 =>
        if (Parameter.Compartment.Static.toSet == compartments.toSet) {
          Seq(this)
        } else {
          for (compartment <- Parameter.Compartment.Static) yield {
            copy(
              Parameter =
                Parameter.copy(
                  Compartment = ModelStaticCompartmentSite(Seq(compartment))
                )
            )
          }
        }

      // Case 2: Complete compartment expansion
      case 2 =>
        for {
          compartment <- Parameter.Compartment.Static
        } yield {
          copy(
            Parameter =
              Parameter.copy(
                Compartment = ModelStaticCompartmentSite(Seq(compartment))
              )
          )
        }
    }
  }

  /**
   * Determine reactant compartments that make the associated rule compartments valid. Constrain the reactant
   * compartments according to them.
   */
  def RuleCompartmentsRemoved: Seq[ModelRule] = {
    Reactants.size match {
      // Case 0: Has no reactant compartments
      case 0 =>
        Seq(this)

      // Case 1: Intersect with current reactant compartment
      case 1 =>
        Seq(
          copy(
            Reactants = Seq(
              Reactants(0).copy(
                Compartment = Parameter.Compartment.copy(
                  Static = Parameter.Compartment.Static.intersect(Reactants(0).Compartment.Static)
                )
              )
            )
          )
        )

      // Case 2: Find all reactant compartment pairs consistent with the rule compartment;
      // expand the rule to all possibilities
      // The compartment of the rule is the largest compartment of the reactants
      case 2 =>
        val rule_compartment = Parameter.Compartment.Static(0)

        val adjacent_compartments = {
          if (rule_compartment.Container == None) {
            rule_compartment.Members
          } else {
            rule_compartment.Members :+ rule_compartment.Container.get
          }
        }

        val smaller_compartments = adjacent_compartments.filter(_.Dimension < rule_compartment.Dimension)

        val compartment_combinations = smaller_compartments.map(smaller => (rule_compartment, smaller))

        val compartment_permutations =
          compartment_combinations ++ compartment_combinations.map(_.swap) :+ (rule_compartment, rule_compartment)

        // Return
        for (pair <- compartment_permutations) yield {
          val reactant1 = Reactants(0).copy(
            Compartment =
              Reactants(0).Compartment.copy(
                Static = Seq(pair._1) intersect Reactants(0).Compartment.Static
              )
          )

          val reactant2 = Reactants(1).copy(
            Compartment =
              Reactants(1).Compartment.copy(
                Static = Seq(pair._2) intersect Reactants(1).Compartment.Static
              )
          )

          // Return
          copy(Reactants = Seq(reactant1, reactant2))
        }
    }
  }

  /**
   * Create the disjoint that will be true iff the pattern compartment is true.
   * This is the case if one of the pattern compartments is present on a monomer and no smaller compartments are present
   */
  def ReactantCompartmentsRemoved(compartments: Seq[ModelCompartment]): ModelRule = {
    this.copy(Reactants = Reactants.map(_.PatternCompartmentsRemoved(compartments)))
  }

  /**
   * 1) Ensure that the disjoint is up-to-date with respect to the main graph. Any disjoint graphs that are definitely
   * true should be replaced by true. Any that are are definitely false should be replaced by false. The whole disjoint
   * then be simplified.
   * 2) Look for a reactant and disjoint leaf that is still uncertain.
   * 2.1) If the disjoint can be made to be true or false based on the main monomers, then refine the main graph to
   * split into the version that is true and the version that is false with respect to that disjoint graph.
   * 2.2) If the disjoint can only be made true or false based on additional monomers than may be reached through
   * ambiguous sites, then refine toward the disjoint by adding the unbound and a bound to every possible monomer out
   * that site
   * 3) Create a rule for each refined reactant
   */
  def DisjointRemoved: Seq[ModelRule] = {
    Reactants.size match {
      // No disjoint to refine
      case 0 =>
        Seq(this)

      // Make one rule for each version of the reactant
      case 1 =>
        val refined_reactants = Reactants(0).DisjointRemoved

        // Return
        for (reactant <- refined_reactants) yield {
          this.copy(Reactants = Seq(reactant))
        }

      // Make a rule for every possible combination of reactant
      case 2 =>
        val refined_reactants = Reactants.map(_.DisjointRemoved)

        // Return
        for {
          reactant1 <- refined_reactants(0)
          reactant2 <- refined_reactants(1)
        } yield {
          this.copy(Reactants = Seq(reactant1,reactant2))
        }
    }
  }

/*
  def RefineReactantsAroundReactant(reactant: ModelReactantGraph): Seq[ModelRule] = {
    val refined_reactants = {
      Reactants.map(this_reactant =>
        // Refine the graph of the reactant to a list of graphs
        this_reactant.Main.RefineConsumedAroundReactant(reactant)
        // Make a copy of the reactant with each new graph
        .map(graph => this_reactant.copy(Main = graph))
      )
    }

    refined_reactants match {
      // No reactants
      case Nil =>
        Seq(this)

      // One reactant
      case Seq(new_reactants) =>
        for (new_reactant <- new_reactants) yield {
          this.copy(Reactants = Seq(new_reactant))
        }

      // Two reactants
      case Seq(first_new_reactants, second_new_reactants) =>
        for {
          first <- first_new_reactants
          second <- second_new_reactants
        } yield {
          this.copy(Reactants = Seq(first, second))
        }
    }
  }
*/

  // This method does not modify the reactant multipliers. The rules are copied once for each alignment already
  def RefineProductAlignmentTowardFragment(fragment: ModelStaticGraph): Seq[ModelRule] = {
    val refined_cores = Core.AllProducingAlignmentsRefinedTowardFragment(fragment)

    Reactants.size match {
      case 0 =>
        // There is no refinement, only multiple alignments; copy for each alignment
        refined_cores.map(ignore => this)

      case 1 =>
        for (core <- refined_cores) yield {
          val reactant_monomers = core.Monomers.filter(_.IsModelReactantMonomer).asInstanceOf[Seq[ModelReactantMonomer]]

          val reactant_edges = core.Edges.collect{case x: ModelReactantBoundEdge => x}

          val reactant_graph = ModelReactantGraph(reactant_monomers, reactant_edges)

          val reactant_modifier = (
            Reactants(0).Modifier
            * reactant_graph.ToStaticReactant.AutomorphismCount
            /  Reactants(0).Main.ToStaticReactant.AutomorphismCount
          )

          val reactant = Reactants(0).copy(Main = reactant_graph, Modifier = reactant_modifier)

          copy(Reactants = Seq(reactant))
        }

      case 2 =>
        val option_rules = {
          for (core <- refined_cores) yield {
            val reactant_monomers = core.Monomers.filter(_.IsModelReactantMonomer).asInstanceOf[Seq[ModelReactantMonomer]]

            val reactant_edges = core.Edges.collect{case x: ModelReactantBoundEdge => x}

            val reactant_graphs = ModelReactantGraph(reactant_monomers, reactant_edges).SplitIntoConnected

            // Return
            reactant_graphs match {
              // The core was refined so that the reactants became linked, this is not a true refinement of the rule
              case Seq(glued) =>
                None

              case Seq(first, second) =>
                val first_modifier = (
                  Reactants(0).Modifier
                  * first.ToStaticReactant.AutomorphismCount
                  /  Reactants(0).Main.ToStaticReactant.AutomorphismCount
                )

                val second_modifier = (
                  Reactants(1).Modifier
                  * second.ToStaticReactant.AutomorphismCount
                  /  Reactants(1).Main.ToStaticReactant.AutomorphismCount
                )
                Some(copy(Reactants = Seq(
                  Reactants(0).copy(Main = first, Modifier = first_modifier),
                  Reactants(1).copy(Main = second, Modifier = second_modifier)
                )))
            }
          }
        }

        // Return
        option_rules.flatten
    }
  }

/*
  def RefineProductAroundFragment(fragment: ModelStaticGraph): Seq[ModelRule] = {
    val refined_cores = Core.RefineProducingAroundFragment(fragment)

    if (refined_cores == Seq(Core)) {
      // No refinement was performed, simply return this rule
      Seq(this)
    } else {
      Reactants.size match {
        case 0 =>
          Seq(this)

        case 1 =>
          for (core <- refined_cores) yield {
            val reactant_monomers = core.Monomers.filter(_.IsModelReactantMonomer).asInstanceOf[Seq[ModelReactantMonomer]]

            val reactant_edges = core.Edges.collect{case x: ModelReactantBoundEdge => x}

            val reactant_graph = ModelReactantGraph(reactant_monomers, reactant_edges)

            val reactant_modifier = (
              Reactants(0).Modifier
              * reactant_graph.ToStaticReactant.AutomorphismCount
              /  Reactants(0).Main.ToStaticReactant.AutomorphismCount
            )

            val reactant = Reactants(0).copy(Main = reactant_graph, Modifier = reactant_modifier)

            copy(Reactants = Seq(reactant))
          }

        case 2 =>
          val option_rules = {
            for (core <- refined_cores) yield {
              val reactant_monomers = core.Monomers.filter(_.IsModelReactantMonomer).asInstanceOf[Seq[ModelReactantMonomer]]

              val reactant_edges = core.Edges.collect{case x: ModelReactantBoundEdge => x}

              val reactant_graphs = ModelReactantGraph(reactant_monomers, reactant_edges).SplitIntoConnected

              // Return
              reactant_graphs match {
                // The core was refined so that the reactants became linked, this is not a true refinement of the rule
                case Seq(glued) =>
                  None

                case Seq(first, second) =>
                  val first_modifier = (
                    Reactants(0).Modifier
                    * first.ToStaticReactant.AutomorphismCount
                    /  Reactants(0).Main.ToStaticReactant.AutomorphismCount
                  )

                  val second_modifier = (
                    Reactants(1).Modifier
                    * second.ToStaticReactant.AutomorphismCount
                    /  Reactants(1).Main.ToStaticReactant.AutomorphismCount
                  )

                  Some(copy(Reactants = Seq(
                    Reactants(0).copy(Main = first, Modifier = first_modifier),
                    Reactants(1).copy(Main = second, Modifier = second_modifier)
                  )))
              }
            }
          }

          // Return
          option_rules.flatten
      }
    }
  }
*/

  override def toString = Reactants.map(_.ToStaticReactant).mkString(" + ") + " -> " + Products.map(_.ToStaticProduct).mkString(" + ") + " " + Parameter
}

object ModelRule {
  def ConvertRule(
    rule: Rule,
    compartments: Seq[ModelCompartment],
    agents: Seq[ModelAgent],
    parameters: Seq[Parameter]
  ) = {
    val forward_rules = {
      for (parameter <- rule.ForwardParameters) yield {
        ModelRule(rule.Name, rule.Reactants, rule.Products.map(_.Main), parameter, compartments, agents, parameters)
      }
    }

    val reverse_rules = {
      for (parameter <- rule.ReverseParameters) yield {
        ModelRule(rule.Name, rule.Products, rule.Reactants.map(_.Main), parameter, compartments, agents, parameters)
      }
    }

    // Return concatenated list
    forward_rules ++ reverse_rules
  }

  def init_rule_edge_map[A](list: Seq[Int]): Map[Int, Buffer[(A,Int,Int)]] =
    Map() ++ list.map(int => (int, ArrayBuffer[(A,Int,Int)]()))

  // Factory to convert from public rule to model rule
  def apply(
    name: Option[String],
    original_reactants: Seq[RulePattern],
    original_products: Seq[RuleGraph],
    original_parameter: RuleParameter,
    compartments: Seq[ModelCompartment],
    agents: Seq[ModelAgent],
    parameters: Seq[Parameter]
  ): ModelRule = {
    // Bins for all sites to register themselves with the edge numbers they posses
    val reactant_edge_map: Map[Int,Buffer[(ModelEdgeSite,Int,Int)]] =
      init_rule_edge_map(original_reactants.flatMap(_.MainBoundEdgeIndexes))

    val product_edge_map: Map[Int,Buffer[(ModelEdgeSite,Int,Int)]]  =
      init_rule_edge_map(original_products.flatMap(_.BoundEdgeIndexes))

    // Convert reactants
    val reactants = {
      original_reactants.map(
        reactant => ModelReactantPattern(reactant, compartments, agents, reactant_edge_map)
      )
    }

    // Order of conserved monomer indexes in reactants and products
    val reactant_index_order =
      original_reactants.map(_.Main).flatMap(_.Monomers).map(_.Index).collect{
        case Some(int) if int > 0 => int
      }

    val product_index_order =
      original_products.flatMap(_.Monomers).map(_.Index).collect{
        case Some(int) if int > 0 => int
      }

    // Which reactant monomers belong to which reactant
    val consumed_reactant_membership = {
      for {
        i_reactant <- 0 until original_reactants.size
        monomer <- original_reactants(i_reactant).Main.Monomers
        if monomer.Index.get == 0
      } yield {
        i_reactant
      }
    }

    val unsorted_conserved_monomer_reactant_membership = {
      for {
        i_reactant <- 0 until original_reactants.size
        monomer <- original_reactants(i_reactant).Main.Monomers
        if monomer.Index.get > 0
      } yield {
        i_reactant
      }
    }

    // Extract conserved monomers from reactants
    val unsorted_conserved_reactant_monomers: Seq[ModelConservedMonomer] =
      reactants.map(_.Main).flatMap(_.Monomers).filter(_.IsModelStaticMonomer).asInstanceOf[Seq[ModelStaticMonomer]]
    val consumed_monomers: Seq[ModelConsumedMonomer] =
      reactants.map(_.Main).flatMap(_.Monomers).filter(_.IsModelConsumedMonomer).asInstanceOf[Seq[ModelConsumedMonomer]]

    // Recreate products
    val products: Seq[ModelProductGraph] =
      // Only the main graphs matter (and only their concatenation, actually)
      original_products.map(
        graph => ModelProductGraph(graph, compartments, agents, product_edge_map)
      )

    // Extract conserved monomers from products
    val unsorted_conserved_product_monomers: Seq[ModelConservedMonomer] =
      products.flatMap(_.Monomers).filter(_.IsModelStaticMonomer).asInstanceOf[Seq[ModelStaticMonomer]]
    val produced_monomers: Seq[ModelProducedMonomer] =
      products.flatMap(_.Monomers).filter(_.IsModelProducedMonomer).asInstanceOf[Seq[ModelProducedMonomer]]

    // Sort conserved monomers by index
    // This aligns the sequences of reactant and product monomers, so that the monomers can be compared
    // to their counterparts on the other side of the rule
    val (
      conserved_reactant_monomers: Seq[ModelConservedMonomer],
      conserved_monomer_reactant_membership: Seq[Int]
    ) = {
      // Zip the monomers, reactant index, and monomer index together
      (unsorted_conserved_reactant_monomers, unsorted_conserved_monomer_reactant_membership, reactant_index_order).zipped.toSeq
      // Sort by the monomer index
      .sortBy(_._3)
      // Unzip
      .unzip3.pipe(tuple3 => (tuple3._1, tuple3._2))
    }

    val conserved_product_monomers = unsorted_conserved_product_monomers.zip(product_index_order).sortBy(_._2).map(_._1)

    // Remove edge indexes to create pairs of sites
    // The sequence of sites at each key should have exactly two tuples in each by now
    val reactant_edge_tuples = reactant_edge_map.values.map(seq => (seq(0), seq(1)))
    val product_edge_tuples  = product_edge_map.values.map(seq => (seq(0), seq(1)))

    // Consumed edges
    val consumed_edge_pairs = {
      // For each tuple pair in the reactants, filter out the pair
      reactant_edge_tuples.filterNot(
        reactant_tuple =>
          // if there exists a tuple pair in the products
          product_edge_tuples.exists(
            product_tuple =>
              // that matches in an unordered fashion over the site pair (monomer and site index tuple is ordered)
              Set((reactant_tuple._1._2, reactant_tuple._1._3), (reactant_tuple._2._2, reactant_tuple._2._3)) ==
              Set((product_tuple._1._2, product_tuple._1._3), (product_tuple._2._2, product_tuple._2._3))
          )
      )
      // Return a BuildEdge for each item filtered
      .map(
        reactant_tuple =>
          ModelConsumedBoundEdge(Seq(reactant_tuple._1._1, reactant_tuple._2._1))
      )
    }

    // Observed edges
    val static_edge_pairs = {
      // For each tuple pair in the reactants, filter in the pair
      reactant_edge_tuples.filter(
        reactant_tuple =>
        // if there exists a tuple pair in the products
          product_edge_tuples.exists(
            product_tuple =>
              // that matches in an unordered fashion over the site pair (monomer and site index tuple is ordered)
              Set((reactant_tuple._1._2, reactant_tuple._1._3), (reactant_tuple._2._2, reactant_tuple._2._3)) ==
              Set((product_tuple._1._2, product_tuple._1._3), (product_tuple._2._2, product_tuple._2._3))
          )
      )
      // Return a BuildEdge for each item filtered
      .map(
        reactant_tuple =>
          ModelStaticBoundEdge(Seq(reactant_tuple._1._1, reactant_tuple._2._1))
      )
    }

    val static_edge_sites = static_edge_pairs.flatMap(_.Targets)

    // Produced edges
    val produced_edge_pairs = {
      // For each tuple pair in the products, filter out the pair
      product_edge_tuples.filterNot(
        product_tuple =>
        // if there exists a tuple pair in the reactant
          reactant_edge_tuples.exists(
            reactant_tuple =>
              // that matches in an unordered fashion over the site pair (monomer and site index tuple is ordered)
              Set((reactant_tuple._1._2, reactant_tuple._1._3), (reactant_tuple._2._2, reactant_tuple._2._3)) ==
              Set((product_tuple._1._2, product_tuple._1._3), (product_tuple._2._2, product_tuple._2._3))
          )
      )
      // Return a ModelEdge for each item filtered
      .map(
        product_tuple =>
          ModelProducedBoundEdge(Seq(product_tuple._1._1, product_tuple._2._1))
      )
    }

    // Compare the compartments and states of all sites of all conserved monomers, changing them to consumed or produced,
    // depending on whether they exist only in the reactants or products
    val conserved_monomers = {
      for (i_monomer <- 0 until conserved_reactant_monomers.size) yield {
        val reactant_monomer = conserved_reactant_monomers(i_monomer)
        val product_monomer = conserved_product_monomers(i_monomer)

        // If reactant states and product states don't match, mark all reactant states as consumed and the product
        // state as produced, even if the product state is a member of the reactant states
        val state_sites = {
          for (i_site <- 0 until reactant_monomer.StateSites.size) yield {
            val reactant_site = reactant_monomer.StateSites(i_site)
            val product_site = product_monomer.StateSites(i_site)

            // Return
            if (reactant_site.Observed != product_site.Observed) {
              ModelChangedStateSite(
                reactant_site.AgentSite,
                reactant_site.Observed,
                product_site.Observed(0) // There can only be one
              )
            }
            else {
              reactant_site
            }
          }
        }

        // If the product is ambiguous, return the reactant, since the site must be static
        // If the product is unbound or bound, return a changed site if the reactant is not the same
        val edge_sites = {
          for (i_site <- 0 until reactant_monomer.EdgeSites.size) yield {
            val reactant_site = reactant_monomer.EdgeSites(i_site)
            val product_site = product_monomer.EdgeSites(i_site)

            // Return
            product_site match {
              // Product is an unbound site
              case ModelStaticLocalEdgeSite(_, Seq(ModelUnboundEdge)) => {
                reactant_site match {
                  case ModelStaticLocalEdgeSite(agent, Seq(ModelUnboundEdge)) =>
                    reactant_site

                  case ModelStaticLocalEdgeSite(agent, consumed) =>
                    ModelChangedLocalUnboundEdgeSite(agent, consumed)

                  case ModelStaticBoundEdgeSite(agent, consumed) =>
                    ModelChangedBoundUnboundEdgeSite(agent, consumed)
                }
              }

              // If the product does not result in unbound or bound, then the site must be static
              case ModelStaticLocalEdgeSite(_, static) => {
                reactant_site match {
                  case ModelStaticLocalEdgeSite(_, `static`) => reactant_site
                }
              }

              // Product is a bound site
              case ModelStaticBoundEdgeSite(_, produced) => {
                reactant_site match {
                  case ModelStaticLocalEdgeSite(agent, consumed) =>
                    ModelChangedLocalBoundEdgeSite(agent, consumed, produced)

                  case ModelStaticBoundEdgeSite(agent, consumed) => {
                    if (static_edge_sites.contains(reactant_site))
                      reactant_site
                    else
                      ModelChangedBoundBoundEdgeSite(agent, consumed, produced)
                  }
                }
              }
            }
          }
        }

        // If reactant compartments do not match product compartments, mark all reactant compartments as consumed
        // and the product compartment as produced
        val compartment = {
          if (reactant_monomer.Compartment.Observed != product_monomer.Compartment.Observed) {
            ModelChangedCompartmentSite(
              reactant_monomer.Compartment.Observed,
              product_monomer.Compartment.Observed(0) // There can only be one
            )
          }
          else {
            reactant_monomer.Compartment
          }
        }

        // Return
        reactant_monomer.copy(StateSites = state_sites, EdgeSites = edge_sites, Compartment = compartment)
      }
    }

    // Remap sites so that edges can be rebuilt
    val reactant_site_remap = {
      val old_monomers = consumed_monomers ++ conserved_reactant_monomers
      val new_monomers = consumed_monomers ++ conserved_monomers

      val old_sites = old_monomers.flatMap(_.EdgeSites)
      val new_sites = new_monomers.flatMap(_.EdgeSites)

      // Return
      old_sites.zip(new_sites).toMap[ModelEdgeSite,ModelReactantEdgeSite]
    }

    val product_site_remap = {
      val old_monomers = conserved_product_monomers ++ produced_monomers
      val new_monomers = conserved_monomers ++ produced_monomers

      val old_sites = old_monomers.flatMap(_.EdgeSites)
      val new_sites = new_monomers.flatMap(_.EdgeSites)

      // Return
      old_sites.zip(new_sites).toMap[ModelEdgeSite,ModelProductEdgeSite]
    }

    val consumed_edges = consumed_edge_pairs.map(_.Targets.map(reactant_site_remap(_)).pipe(ModelConsumedBoundEdge(_)))
    val static_edges = static_edge_pairs.map(_.Targets.map(reactant_site_remap(_)).pipe(ModelStaticBoundEdge(_)))
    val produced_edges = produced_edge_pairs.map(_.Targets.map(product_site_remap(_)).pipe(ModelProducedBoundEdge(_)))

    // Rebuild reactants by extracting the corresponding monomers and edges
    val recreated_reactants = {
      for (i_reactant <- 0 until reactants.size) yield {
        val reactant = reactants(i_reactant)

        // Filter in the monomers that originally came from this reactant
        val new_consumed_monomers = {
          consumed_monomers.zip(consumed_reactant_membership)
          .filter(_._2 == i_reactant)
          .map(_._1)
        }

        val new_conserved_monomers = {
          conserved_monomers.zip(conserved_monomer_reactant_membership)
          .filter(_._2 == i_reactant)
          .map(_._1)
        }

        val monomers = new_consumed_monomers ++ new_conserved_monomers

        // Filter in the edges that connects to these monomers
        val edges = (consumed_edges ++ static_edges).filter(edge => monomers.exists(_.BoundEdgeConnectsHere(edge)))

        // Return
        reactant.copy(Main = ModelReactantGraph(monomers, edges))
      }
    }

    // Convert parameter
    val parameter = ModelRuleParameter(original_parameter, compartments, parameters)

    // Construct the rule
    ModelRule(name, recreated_reactants, produced_monomers, produced_edges.toSeq, parameter).BoundEdgeSitesRebuilt
  }
}
