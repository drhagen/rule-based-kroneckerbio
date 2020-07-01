/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

import kroneckerbio.model._
import kroneckerbio.model.refining.refining._
import kroneckerbio.utilities.Inquirable._
import kroneckerbio.utilities.Rational._
import kroneckerbio.utilities.RichAny._
import kroneckerbio.utilities.RichSeq._
import collection.mutable.Buffer

final case class ModelPatternClass[
  +MainType <: ModelReactantGraph
](
  Main: MainType,
  Disjoint: Inquirable[ModelStaticGraph],
  Compartment: ModelStaticCompartmentSite,
  Modifier: Rational = 1
) {

  def Reduced(
    edge_site_tuples: Buffer[(ModelEdgeSite,ModelEdgeSite)] = Buffer[(ModelEdgeSite,ModelEdgeSite)]()
  ) = {
    copy(
      Main = Main.Reduced(edge_site_tuples),
      Disjoint = Disjoint.replaceLeavesBy(leaf => leaf.Reduced())
    )
  }

  def ToStaticReactant = ModelPatternClass(Main.ToStaticReactant, Disjoint, Compartment)

  // Does not modify main monomers or edges
  def PatternCompartmentsRemoved(compartments: Seq[ModelCompartment]): ModelPatternClass[MainType] = {
    if (Compartment.Static.toSet == compartments.toSet) {
      // Totally ambiguous compartments are always satisfied, no need for disjoint context
      this
    } else {
      val reachable_monomers = Main.ReachableMonomers

      // Create an inquirable for each pattern compartment
      val pattern_compartment_contracts =
        for (compartment <- Compartment.Static) yield {
          // Inquirable for asserting that one correct compartment is true
          val correct_inquirable =
            construct_correct_disjoint(reachable_monomers, compartment)

          // Inquirable for asserting that no smaller compartments are true
          val smaller_inquirable =
            construct_smaller_disjoint(reachable_monomers, compartment)

          // Both must be true for this compartment to be true, so wrap in an And
          And(correct_inquirable, smaller_inquirable)
        }

      // Only of the compartments must be true, so wrap in an Or
      val reactant_compartment_disjoint = Or(pattern_compartment_contracts)

      // Return the updated disjoint context so that it must meet the original, correct, and smaller contracts
      copy(Disjoint = And(Disjoint, reactant_compartment_disjoint).simplified)
    }
  }

  private def construct_correct_disjoint(
    reachables: Seq[ModelAgent],
    compartment: ModelCompartment
  ) = {
    val pattern_monomers = Main.Monomers

    val reachables_with_this_compartment =
      reachables.filter(_.Compartments.contains(compartment))

    val monomers_with_this_compartment =
      pattern_monomers.filter(_.Compartment.Observed.contains(compartment))

    val agents_with_this_compartment =
      (reachables_with_this_compartment ++ monomers_with_this_compartment.map(_.Agent)).distinct

    // Construct single-monomer graphs out of the agents
    // Return
    val correct_monomers =
      for (agent <- agents_with_this_compartment) yield {
        ModelStaticMonomer(
          Agent = agent,
          Compartment = ModelStaticCompartmentSite(Seq(compartment))
        )
      }

    // Convert to graphs, which is what the disjoint is
    val correct_graphs = correct_monomers.map(monomer => ModelStaticGraph(Seq(monomer), Nil))

    // Turn each graph into an inquirable leaf and wrap the whole thing in an Or
    // Return
    correct_graphs.map(InquirableLeaf(_)).pipe(Or(_))
  }

  private def construct_smaller_disjoint(
    reachables: Seq[ModelAgent],
    compartment: ModelCompartment
  ) = {
    val pattern_monomers = Main.Monomers

    val reachables_with_smaller_compartment =
      reachables.filter(_.Compartments.exists(_.Dimension < compartment.Dimension))

    val monomers_with_smaller_compartment =
      pattern_monomers.filter(_.Compartment.Observed.exists(_.Dimension < compartment.Dimension))

    val agents_with_smaller_compartment =
      (reachables_with_smaller_compartment ++ monomers_with_smaller_compartment.map(_.Agent)).distinct

    // Filter for agents that can be in a smaller, adjacent compartment
    val smaller_monomers =
      for (agent <- agents_with_smaller_compartment) yield {
        ModelStaticMonomer(
          Agent = agent,
          Compartment = ModelStaticCompartmentSite(
            agent.Compartments.filter(_.Dimension < compartment.Dimension)
          )
        )
      }

    // Convert to graphs, which is what the disjoint is
    val smaller_graphs = smaller_monomers.map(monomer => ModelStaticGraph(Seq(monomer), Nil))

    // Turn each graph into an inquirable leaf and wrap the whole thing in a Nor
    // Return
    smaller_graphs.map(InquirableLeaf(_)).pipe(Nor(_))
  }

  def DisjointRemoved: Seq[ModelPatternClass[MainType]] = {
    // Remove any disjoints that are necessarily true or false, and simplify the resulting tree
    val disjoint = Disjoint.graftBranchesBy(leaf => Main.SimplifyDisjoint(leaf)).simplified

    // Return
    disjoint match {
      case True =>
        Seq(copy(Disjoint = disjoint))
      case False =>
        Nil
      case _ => {
        // Get one of the leaves for which refinement will be helpful
        //#TODO: There are more intelligent ways to choose the next graph to refine
        val disjoint_doing_refinement = disjoint.leaves(0)

        // See if local refinement will be sufficient
        val option_local_context_to_refine = Main.ReactantNeedsLocalRefinementForContract(disjoint_doing_refinement)

        option_local_context_to_refine match {
          //#TODO: more match options need to be considered if more complex disjoint is allowed
          // Refine the local graph if it needs refined
          case (Seq(compartment_to_refine), Nil, Nil) => {
            // Find index of monomer being refined
            val (monomer_being_refined, monomer_being_refined_index) = (
              Main.Monomers.zipWithIndex
              .FindOrDie(_._1.Compartment == compartment_to_refine._1)
            )

            // Refine toward
            val toward_graph = {
              Main.copy(
                Monomers = Main.Monomers.updated(
                  monomer_being_refined_index, monomer_being_refined.RefineCompartmentToward(compartment_to_refine._2)
                )
              )
              .asInstanceOf[MainType]
            }

            val toward_modifier =
              Modifier * toward_graph.ToStaticReactant.AutomorphismCount / Main.ToStaticReactant.AutomorphismCount

            val toward_pattern = copy(Main = toward_graph, Modifier = toward_modifier)

            // Refine against
            val against_graph = {
              Main.copy(
                Monomers = Main.Monomers.updated(
                  monomer_being_refined_index, monomer_being_refined.RefineCompartmentAgainst(compartment_to_refine._2)
                )
              )
              .asInstanceOf[MainType]
            }

            val against_modifier =
              Modifier * against_graph.ToStaticReactant.AutomorphismCount / Main.ToStaticReactant.AutomorphismCount

            val against_pattern = copy(Main = against_graph, Modifier = against_modifier)

            // Return recursively
            Seq(toward_pattern, against_pattern)
            .filter(_.Main.ToStaticReactant.GraphCanExist)
            .map(_.DisjointRemoved)
            .flatten
          }

          // Otherwise, refine by adding a monomer out a site
          case (Nil, Nil, Nil) => {
            // This has to be found if we got this far
            val site_to_refine = Main.ReactantNeedsRemoteRefinementForContract(disjoint_doing_refinement).get

            // Refine toward
            val new_reactant_graphs =
              Main.RefineEdgeSiteTowardBoundMonomer(site_to_refine._1, site_to_refine._2).asInstanceOf[Seq[MainType]]

            val toward_patterns = {
              for (toward_graph <- new_reactant_graphs) yield {
                val toward_modifier =
                  Modifier * toward_graph.ToStaticReactant.AutomorphismCount / Main.ToStaticReactant.AutomorphismCount

                copy(Main = toward_graph, Modifier = toward_modifier)
              }
            }

            // Refine against
            val against_graph =
              Main.RefineEdgeSiteAgainstBound(site_to_refine._1, site_to_refine._2).asInstanceOf[MainType]

            val against_modifier =
              Modifier * against_graph.ToStaticReactant.AutomorphismCount / Main.ToStaticReactant.AutomorphismCount

            val against_pattern = copy(Main = against_graph, Modifier = against_modifier)

            // Return recursively
            (toward_patterns :+ against_pattern)
            .filter(_.Main.ToStaticReactant.GraphCanExist)
            .map(_.DisjointRemoved)
            .flatten
          }
        }
      }
    }
  }

  override def toString = (
    ""
    + Main
    + (if (Disjoint != True) {
        ".{" + Disjoint + "}"
      }
      else {
        ""
      })
    + "." + Compartment
  )
}

object ModelReactantPattern {
  def apply(main: ModelReactantGraph, disjoint: Inquirable[ModelStaticGraph], compartment: ModelStaticCompartmentSite) =
    new ModelPatternClass(main, disjoint, compartment)

  def apply(
    original: RulePattern,
    compartments: Seq[ModelCompartment],
    agents: Seq[ModelAgent],
    reactant_edge_map: Map[Int,Buffer[(ModelEdgeSite,Int,Int)]]
  ): ModelRulePattern = {
    // Recreate main graph
    val main = ModelReactantGraph(original.Main, compartments, agents, reactant_edge_map)

    // Recreate leaf graphs and replace leaves of Inquirable
    val disjoint = {
      original.Disjoint.replaceLeaves(
        // Create list of tuples of leaves and new leaves
        original.Disjoint.leaves.map(
          leaf => (leaf, ModelStaticGraph(leaf, compartments, agents))
        )
        // Convert tuple list to map so it can replace the old graphs
        .toMap
      )
    }

    // Resolve pattern compartment
    val pattern_compartments = ModelStaticCompartmentSite(original.Compartment, compartments)

    // Return
    ModelReactantPattern(main, disjoint, pattern_compartments)
  }
}

object RefineObservedPattern {
  def apply(original: ObservablePattern, compartments: Seq[ModelCompartment], agents: Seq[ModelAgent]) = {
    // Replace main graph
    val main = ModelStaticGraph(original.Main, compartments, agents)

    // Recreate leaf graphs and replace leaves of Inquirable
    val disjoint = {
      original.Disjoint.replaceLeaves(
        // Create list of tuples of leaves and new leaves
        original.Disjoint.leaves.map(
          leaf => (leaf, ModelStaticGraph(leaf, compartments, agents))
        )
        // Convert tuple list to map so it can replace the old graphs
        .toMap
      )
    }

    // Resolve pattern compartment
    val pattern_compartments = ModelStaticCompartmentSite(original.Compartment, compartments)

    // Return
    ModelPatternClass(main, disjoint, pattern_compartments)
  }
}
