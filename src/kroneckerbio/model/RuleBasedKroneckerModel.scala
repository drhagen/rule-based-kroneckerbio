/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model

import scala.collection.mutable.ArrayBuffer
import java.io.FileWriter
import kroneckerbio.utilities.ErrorBuffer._
import kroneckerbio.implicitOption
import kroneckerbio.utilities.RichSeq._
import kroneckerbio.utilities.WatchableBuffer
import kroneckerbio.model.refining._
import kroneckerbio.model.refining.refining._
import kroneckerbio.utilities.Inquirable._
import kroneckerbio.utilities.Rational._

class RuleBasedKroneckerModel(name: String = "") {
  //#TODO: Make the model components vars that invalidate the model on write
  var Name: String = name
  val Compartments = new WatchableBuffer[Compartment](ArrayBuffer(), () => topology_modified())
  val Agents = new WatchableBuffer[Agent](ArrayBuffer(), () => topology_modified())
  val Seeds = new WatchableBuffer[Seed](ArrayBuffer(), () => topology_modified())
  val Observables = new WatchableBuffer[Observable](ArrayBuffer(), () => topology_modified())
  val Parameters = new WatchableBuffer[Parameter](ArrayBuffer(), () => topology_modified())
  val Rules = new WatchableBuffer[Rule](ArrayBuffer(), () => topology_modified())

  var model_seeds: Seq[ModelSeed] = Nil
  var model_fragments: Seq[ModelFragment] = Nil
  var model_observables: Seq[ModelObservable] = Nil
  var model_contributor_map: Map[ModelContributor, ModelFragmentCluster] = Map()

  // Component names
  def CompartmentNames = Compartments.map(_.Name)

  // Finalization methods
  var model_valid = false
  var topology_valid = false
  var parameters_valid = false

  def topology_modified() {
    parameters_valid = false
    topology_valid = false
    model_valid = false
  }

  def parameters_modified() {
    parameters_valid = false
  }

  def AgentSiteStates(Monomer: String, Site: String) = {
    Agents.find(_.Name == Monomer).get.Sites.find(_.Name == Site).get.States
  }

  // Validation methods
  def Validate(crash: Boolean = true): Boolean = {
    if (model_valid == true) {
      // No sense in checking everything again if it is still correct
      return true
    }

    // Create error buffer to collect all errors at once
    val errors = new ErrorBuffer("Model \"" + Name + "\" is invalid:")

    // Ensure that all the names are unique
    //#todo Decide if repetition should be banned or overwritten
    //#todo Decide if rule name duplication should be be banned
    errors ++ (new ErrorBuffer("Compartment names are not unique:") ++ assertNoDuplicates(Compartments.map(_.Name)))
    errors ++ (new ErrorBuffer("Agent names are not unique:") ++ assertNoDuplicates(Agents.map(_.Name)))
    errors ++ (new ErrorBuffer("Observable names are not unique:") ++ assertNoDuplicates(Observables.map(_.Name)))
    errors ++ (new ErrorBuffer("Parameter names are not unique:") ++ assertNoDuplicates(Parameters.map(_.Name)))
    errors ++ (new ErrorBuffer("Rule names are not unique (rules don't need names):") ++ assertNoDuplicates(Rules.map(_.Name).flatten))

    // Validate that compartments' containers form a tree
    // Extract all compartments with no container
    var valid_compartment_names = Compartments.filter(_.Container == None).map(_.Name)

    // Extract compartments that have a container
    var invalid_compartments = Compartments.filter(_.Container != None)

    // Flag to stop when no more compartments were found
    var additional_compartments_found = !valid_compartment_names.isEmpty

    while (additional_compartments_found) {
      // Extract compartments that are inside old valid compartments
      val new_valid_compartment_names = invalid_compartments.filter(x => valid_compartment_names.contains(x.Container.get)).map(_.Name)

      // Extract compartments that did not have a container
      invalid_compartments = invalid_compartments.filter(x => !valid_compartment_names.contains(x.Container.get))

      // Update valid compartments
      valid_compartment_names = new_valid_compartment_names
      additional_compartments_found = !valid_compartment_names.isEmpty
    }

    // Assert that all compartments have a container or None
    for (invalid_compartment <- invalid_compartments) {
      if (Compartments.map(_.Name).contains(invalid_compartment.Container)) {
        errors ++ ConcreteError("Compartment \"" + invalid_compartment.Name + "\" is a member of a container cycle")
      }
      else {
        errors ++ ConcreteError("Compartment \"" + invalid_compartment.Name + "\" has a container \"" + invalid_compartment.Container + "\" that does not exist")
      }
    }

    // Validate that all seeds match the agents and compartments
    for (i <- 0 until Seeds.length) {
      errors ++ (
        ErrorBuffer("Seed #" + i + " " + Seeds(i) + " is invalid:")
        ++ validateGraph(Seeds(i).Graph)
        ++ Seeds(i).Graph.Monomers.map{monomer =>
          Agents.find(_.Name == monomer.Name) match {
            case Some(agent) => agent.validateUnambiguousMonomer(monomer)
            case None => NoError() // Already dealt with by validateGraph
          }
        }
      )
    }

    // Validate that all observables match the agents and compartments
    for (i <- 0 until Observables.length) {
      val observableError = ErrorBuffer("Observable #" + i + " \"" + Observables(i).Name + "\" is invalid:")

      for (pattern <- Observables(i).PatternsOnly) {
        observableError ++ validatePattern(pattern.Pattern)
      }

      errors ++ observableError
    }

    // Validate that all reactants and products match the agents and compartments
    for (i <- 0 until Rules.length) {
      val ruleError = ErrorBuffer("Rule #" + i + " \"" + Rules(i).Name.getOrElse("") + "\" is invalid:")

      for (reactant <- Rules(i).Reactants) {
        ruleError ++ validatePattern(reactant)
      }

      for (product <- Rules(i).Products) {
        ruleError ++ validatePattern(product)
      }

      for (parameter <- List(Rules(i).ForwardParameters, Rules(i).ReverseParameters).flatten) {
        ruleError ++ validateParameter(parameter)
      }

      errors ++ ruleError
    }

    // Report errors
    model_valid = !errors.check(crash)

    // Return
    !model_valid
  }

  def validateGraph(graph: Graph) = {
    // Errors from matching monomer to agents
    val agentErrors =
      for (monomer <- graph.Monomers) yield (
        Agents.find(_.Name == monomer.Name) match {
          case None => ConcreteError("Monomer \"" + monomer.Name + "\" not found in Agents")
          case Some(matchingAgent) => matchingAgent.validateMonomer(monomer)
        }
      )

    // Errors from matching monomer compartments
    val compartmentErrors = {
      for {
        monomer <- graph.Monomers
        compartment <- monomer.Compartment.AllCompartmentNames
      } yield {
        Compartments.find(_.Name == compartment) match {
          case None => ConcreteError("Monomer compartment \"" + compartment + "\" not found in Compartments")
          case _ => NoError()
        }
      }
    }

    //#TODO: validate edges
    // Return
    agentErrors ++ compartmentErrors
  }

  def validatePattern(pattern: Pattern) =
    validateGraph(pattern.Main) ++ pattern.Disjoint.leaves.flatMap(validateGraph(_))

  def validateParameter(parameter: RuleParameter) = ConcreteError(
    "Rule parameter \"" + parameter.Name + "\" does not exist in Parameters",
    !Parameters.exists(_.Name == parameter.Name)
  )

  def assertNoDuplicates[A](seq: Seq[A]) = seq.duplicates match {
    case Seq() => NoError()
    case duplicates => ConcreteError(duplicates.mkString(","))
  }

  def Finalize() {
    // Do not finalize an invalid model
    Validate()

    /////
    // Convert components to a form the model can use
    /////
    // Create an initial representation of the model
    val compartments = ModelCompartment.BuildCompartments(Compartments)

    println("")
    println("Compartments")
    compartments.map(println(_))

    val agents = ModelAgent.BuildAgents(Agents, compartments)

    println("")
    println("Agents")
    agents.map(println(_))

    val seeds = Seeds.map(seed => ModelSeed(seed, compartments, agents))

    println("")
    println("Seeds")
    seeds.map(println(_))

    val initial_observables = Observables.map(observable => ModelObservable(observable, compartments, agents))

    println("")
    println("Observables")
    initial_observables.map(println(_))

    val initial_rules = Rules.flatMap(rule => ModelRule.ConvertRule(rule, compartments, agents, Parameters))

    println("")
    println("Rules")
    initial_rules.map(println(_))

    // Reduce the agent compartment, states, and edges to reflect the possibilities according to the seeds and rules
    ModelAgent.ReduceAgents(agents, seeds, initial_rules)

    println("")
    println("Reduced Agents")
    agents.map(println(_))

    // Reduce compartments, states, and edges to those permitted by the seeds and rules
    val reduced_observables = initial_observables.map(_.Reduced(compartments))
    val reduced_rules = initial_rules.map(_.Reduced(compartments))

    println("")
    println("Reduced Observables")
    reduced_observables.map(println(_))

    println("")
    println("Reduced Rules")
    reduced_rules.map(println(_))

    // Determine second-order coupling between sites on the same monomer
    for {
      agent <- agents
      site <- agent.Sites
    } {
      site.SetAdjacentSites(seeds, reduced_rules)
    }

    ////
    // Simplification of rules and observables
    ////
    // Refine multi-rule-compartment rules to a single compartment
    val one_rule_compartment_rules = reduced_rules.flatMap(_.RuleCompartmentsRefined(compartments))

    println("")
    println("One Rule Compartment Rules")
    one_rule_compartment_rules.map(println(_))

    // Refine rule compartments to reactant compartments
    val one_reactant_compartment_rules = one_rule_compartment_rules.flatMap(_.RuleCompartmentsRemoved)

    println("")
    println("One Reactant Compartment Rules")
    one_reactant_compartment_rules.map(println(_))

    // Purge rules and observables with empty pattern compartments
    val no_zero_contributor_compartment_observables =
      for (observable <- reduced_observables) yield {
        val valid_contributors = observable.Contributors.filter(!_.Pattern.Compartment.Static.isEmpty)

        // Return
        observable.copy(Contributors = valid_contributors)
      }

    val no_zero_reactant_compartment_rules = {
      one_reactant_compartment_rules.filterNot(
        _.Reactants.exists(_.Compartment.Static.isEmpty)
      )
    }

    // Turn reactant compartments into disjoint context
    val only_disjoint_observables = no_zero_contributor_compartment_observables.map(_.PatternCompartmentsRemoved(compartments))
    val only_disjoint_rules = no_zero_reactant_compartment_rules.map(_.ReactantCompartmentsRemoved(compartments))

    println("")
    println("Only Disjoint Observables")
    only_disjoint_observables.map(println(_))

    println("")
    println("Only Disjoint Rules")
    only_disjoint_rules.map(println(_))

    // Purge rules and observables that have false disjoints
    val no_false_disjoint_observables =
      for (observable <- only_disjoint_observables) yield {
        val valid_contributors = observable.Contributors.filter(x => x.Pattern.Disjoint != False && x.Pattern.Main.GraphCanExist)

        // Return
        observable.copy(Contributors = valid_contributors)
      }

    val no_false_disjoint_rules = only_disjoint_rules.filterNot(_.Reactants.exists(_.Disjoint == False))

    println("")
    println("No False Disjoint Observables")
    no_false_disjoint_observables.map(println(_))

    println("")
    println("No False Disjoint Rules")
    no_false_disjoint_rules.map(println(_))

    // Turn disjoint into explicit context
    val only_main_observables = no_false_disjoint_observables.map(_.DisjointRemoved)
    val only_core_rules = no_false_disjoint_rules.flatMap(_.DisjointRemoved)

    println("")
    println("Only Explicit Observables")
    only_main_observables.map(println(_))

    println("")
    println("Only Explicit Rules")
    only_core_rules.map(println(_))

    ////
    // Fragmentation
    ////
    // Stores all fragment clusters that have been found to be needed
    var fragment_clusters = Seq[ModelFragmentCluster]()
    // Stores fragment clusters that have not yet been processed
    var new_fragment_clusters = Seq[ModelFragmentCluster]()
    // Stores all fragments that have been found to be needed
    var all_fragments = Seq[ModelFragment]()

    // Function that returns a cluster matching the desired contract, either an existing one or a new one
    def new_cluster(contract: ModelStaticGraph) = {
      // Find it if it exists
      val option_matching_fragment = fragment_clusters.find(_.Main.IdenticalContract(contract))

      // Return
      option_matching_fragment match {
        // It already exists, return it
        case Some(old_cluster) =>
          old_cluster

        // This is a new fragment cluster
        case None =>
          // Make a new cluster
          val cluster = ModelFragmentCluster(contract, Nil)
          // Add it to the list of fragment clusters
          fragment_clusters :+= cluster
          new_fragment_clusters :+= cluster
          // Return the cluster for this contributor
          cluster
      }
    }

    // Initialize fragment cluster collection with observable contributors
    val contributor_fragment_map = {
      (for {
        observable <- only_main_observables
        contributor <- observable.Contributors
      } yield {
        val cluster = new_cluster(contributor.Pattern.Main)

        // Return
        (contributor, cluster)
      })
      .toMap
    }

    // Process the new fragments. Find next fragments required to know the latest new fragments
    // Terminate when no unique fragments are found
    while (new_fragment_clusters.size > 0) {
      println("New fragment clusters: " + new_fragment_clusters.size)
      println("Total fragment clusters: " + fragment_clusters.size)

      // Store the new fragments from last time
      val last_fragment_clusters = new_fragment_clusters

      // Reset the new fragment groups buffer
      new_fragment_clusters = Nil

      for (current_cluster <- last_fragment_clusters) {
        /////
        // Refinement according to consumption
        /////
        // Collect the rules that consume each fragment and which reactant, if any, that participates
        var fragment_tuples: Seq[(ModelStaticGraph, Rational)] =
          Seq((current_cluster.Main, 1))

        var old_fragment_tuples = Seq[(ModelStaticGraph, Rational)]()

        while(fragment_tuples != old_fragment_tuples) {
          // Save old fragments for comparison
          old_fragment_tuples = fragment_tuples

          for {
            rule <- only_core_rules
            reactant <- rule.Reactants
          } {
            fragment_tuples = {
              (for {
                (fragment, modifier) <- fragment_tuples
              } yield {
                val refined_fragments =
                  fragment.RefineConsumedAroundReactant(reactant.Main).asInstanceOf[Seq[ModelStaticGraph]]

                for (new_fragment <- refined_fragments) yield {
                  val new_modifier = modifier * new_fragment.AutomorphismCount / fragment.AutomorphismCount

                  // Return
                  if (new_fragment.GraphCanExist) {
                    Some((new_fragment, new_modifier))
                  } else {
                    None
                  }
                }
              })
              .flatten
              .flatten
            }
          }
        }

        /////
        // Collect rules and add reactants as new clusters
        /////
        val consumption_production_tuples = {
          for ((graph, modifier) <- fragment_tuples) yield {
            // Collect consumption rules
            val consumption_rules = {
              for {
                rule <- only_core_rules
                reactant <- rule.Reactants
                if reactant.Main.IsConsuming(graph)
              } yield {
                val option_other_reactant = rule.Reactants.diff(Seq(reactant)).headOption

                val fragment_cluster_for_other_reactant =
                  option_other_reactant.map(other_reactant => new_cluster(other_reactant.Main.ToStaticReactant))

                // Return
                ModelFragmentConsumptionRule(
                  rule.Parameter.Name,
                  reactant.Main,
                  rule.Reactants.map(_.Modifier).product,
                  fragment_cluster_for_other_reactant
                )
              }
            }

            // Collect production rules
            val production_rules = {
              for {
                rule <- only_core_rules
                refined_rule <- rule.RefineProductAlignmentTowardFragment(graph)
                if refined_rule.Reactants.forall(reactant => reactant.Main.ToStaticReactant.GraphCanExist)
              } yield {
                // Add each reactant as a new fragment group
                val fragment_clusters_for_reactants = {
                  for (reactant <- refined_rule.Reactants) yield {
                    new_cluster(reactant.Main.ToStaticReactant)
                  }
                }

                // Return
                ModelFragmentProductionRule(
                  refined_rule.Parameter.Name,
                  refined_rule.Core.ToProduct,
                  refined_rule.Reactants.map(_.Modifier).product / graph.AutomorphismCount,
                  fragment_clusters_for_reactants
                )
              }
            }
/*
            val production_rules = {
              for {
                rule <- only_core_rules
                refined_rule <- rule.RefineProductAroundFragment(graph)
                if refined_rule.Core.IsProducing(graph)
              } yield {
                // Add each reactant as a new fragment group
                val fragment_clusters_for_reactants = {
                  for (reactant <- refined_rule.Reactants) yield {
                    new_cluster(reactant.Main.ToStaticReactant)
                  }
                }

                // Return
                ModelFragmentProductionRule(
                  refined_rule.Parameter.Name,
                  refined_rule.Core.ToProduct,
                  refined_rule.Reactants.map(_.Modifier).product,
                  fragment_clusters_for_reactants
                )
              }
            }
*/

            // Return
            (graph, modifier, consumption_rules, production_rules)
          }
        }

        /////
        // Assemble fragments and fragment clusters
        /////
        // Add fragments to current fragment cluster
        // Do not create repeated fragments
        current_cluster.Fragments = {
          for {
            (fragment, modifier, consumption_rules, production_rules) <- consumption_production_tuples
          } yield {
            //#todo: check fragment list that was just created also
            val option_matching_fragment = all_fragments.find(_.Main.IdenticalContract(fragment))

            // Return
            option_matching_fragment match {
              case Some(found_fragment) =>
                (found_fragment, modifier)

              case None =>
/*
                // Find initial condition of this fragment
                val matching_seeds = seeds.map(seed => seed.Graph.MatchesCount(fragment) * seed.Value)
                val initial_value = matching_seeds.sum
*/
                // Find compartment of this fragment
                val possible_compartments = fragment.PossibleGraphCompartments

                val fragment_compartment = {
                  possible_compartments match {
                    case Seq(single) => Some(single)
                    case _ => None
                  }
                }

                // Construct new fragment
                val new_fragment = ModelFragment(fragment, fragment_compartment, consumption_rules, production_rules)

                // Add it to the list and return
                all_fragments :+= new_fragment
                (new_fragment, modifier)
            }
          }
        }
      }

      println("Total fragments: " + all_fragments.size)
      println("")
      // Loop while there are new fragment groups to process
    }

    /////
    // Replace fragments that are already represented by other fragments with those other fragments
    /////
    // Return fragments that sum up to this fragment and the appropriate modifiers, or return None if no summing set can be found
    val time = System.nanoTime()
/*    // Old version
      def find_representative_fragments(fragment: ModelStaticGraph, candidates: Seq[ModelFragment]): Option[Seq[ModelFragment]] = {
      // Candidates that don't obey this fragment won't obey the next, just filter them out for speed
      val valid_candidates = candidates.filter(candidate => candidate.Main.ObeysContract(fragment))

      // Look for a candidate that matches this fragment's contract exactly
      val option_identical_candidate = valid_candidates.find(candidate => fragment.ObeysContract(candidate.Main))
      option_identical_candidate match {
        // Exact fragment found
        case Some(identical_fragment) =>
          Some(Seq(identical_fragment))

        // Search for the information spread across many fragments
        case None =>
          // Start from all possible alignments of the candidates with the fragment
          // Construct new graphs so that the fragment can be refined toward the alignment (this may not be necessary if refinement is does stepwise)
          val all_aligned_candidates = {
            for {
              candidate <- valid_candidates
              monomer_ordering <- candidate.Main.FullAlignments(fragment)
            } yield {
              // Align the monomers so that each unaligned monomer is connected to a monomer before it
              var new_monomer_ordering = monomer_ordering
              var unaligned_unsorted_monomers = candidate.Main.Monomers.diff(monomer_ordering)

              for (i <- 0 until unaligned_unsorted_monomers.size) {
                val monomer_to_transfer = unaligned_unsorted_monomers.FindOrDie(
                  monomer => !candidate.Main.AdjacentMonomers(monomer).intersect(new_monomer_ordering).isEmpty
                )

                new_monomer_ordering = new_monomer_ordering :+ monomer_to_transfer
                unaligned_unsorted_monomers = unaligned_unsorted_monomers.diff(Seq(monomer_to_transfer))
              }

              candidate.Main.copy(Monomers = new_monomer_ordering)
            }
          }

          // Refine the fragment toward each alignment
          val all_refinement_groups = {
            for (aligned_candidate <- all_aligned_candidates) yield {
              fragment.RefineFullAlignmentsAroundFragment(aligned_candidate)
            }
          }

          // Recurse to determine if the information in each refinement is already contained in the candidates
          val containing_fragments_groups = {
            for (refinement_group <- all_refinement_groups) yield {
              for (refined_fragment <- refinement_group) yield {
                find_representative_fragments(refined_fragment, valid_candidates)
              }
            }
          }

          // Find a group where all refinements were found
          val complete_containing_fragments_group = containing_fragments_groups.find(_.forall(_ != None))

          // These fragments contain equivalent information to this fragment, return
          complete_containing_fragments_group match {
            // Failure, not all information was found
            case None =>
              None

            // Success, all refinements were already found in the fragments
            case Some(nested_fragments) =>
              Some(nested_fragments.flatten.flatten)
          }
      }
    }

    // Map fragments that need replaced with the fragments replacing them
    var fragments_to_replace = {
      (for {
        fragment <- all_fragments
        option_replacement_fragments = find_representative_fragments(fragment.Main, all_fragments.diff(Seq(fragment)))
      } yield {
        println("On fragment " + fragment.toString)
        (fragment, option_replacement_fragments)
      })
      .collect{case (fragment, Some(replacement_fragments)) => (fragment, replacement_fragments)}
    }*/
    // Assume that the fragment obeys each candidate
    def find_representative_fragments(fragment: ModelStaticGraph, candidates: Seq[ModelFragment]): Option[Seq[ModelFragment]] = {
      // Look for a candidate that matches this fragment's contract exactly
      candidates.find(candidate => fragment.ObeysContract(candidate.Main)) match {
        // Exact fragment found
        case Some(identical_fragment) =>
          Some(Seq(identical_fragment))

        // Search for the information spread across many fragments
        case None =>
          // Start from all possible alignments of the candidates with the fragment
          // Construct new graphs so that the fragment can be refined toward the alignment (this may not be necessary if refinement is does stepwise)
          // #todo: Do we really need to do all alignments? Won't refinement eventually fill out the entire candidate anyway?
          val aligned_candidates = {
            for {
              candidate <- candidates
              monomer_ordering <- candidate.Main.FullAlignments(fragment)
            } yield {
              // Align the monomers so that each unaligned monomer is connected to a monomer before it
              var new_monomer_ordering = monomer_ordering
              var unaligned_unsorted_monomers = candidate.Main.Monomers.diff(monomer_ordering)

              for (i <- 0 until unaligned_unsorted_monomers.size) {
                val monomer_to_transfer = unaligned_unsorted_monomers.FindOrDie(
                  monomer => !candidate.Main.AdjacentMonomers(monomer).intersect(new_monomer_ordering).isEmpty
                )

                new_monomer_ordering = new_monomer_ordering :+ monomer_to_transfer
                unaligned_unsorted_monomers = unaligned_unsorted_monomers.diff(Seq(monomer_to_transfer))
              }

              candidate.Main.copy(Monomers = new_monomer_ordering)
            }
          }

          // Refine the fragment toward each alignment
          // #todo: Wouldn't it be faster to make the list of refinements unique so aren't doing the same refinements over and over
          val refinement_groups = {
            for (aligned_candidate <- aligned_candidates) yield {
              fragment.RefineFullAlignmentsAroundFragment(aligned_candidate)
            }
          }

          // Determine if the information in each refined fragment is already contained in the candidates
          val containing_fragments_groups = {
            for (refinement_group <- refinement_groups) yield {
              // First filter the candidates for each refined fragment
              val all_valid_candidates = {
                for (refined_fragment <- refinement_group) yield {
                  candidates.filter(candidate => candidate.Main.ObeysContract(fragment))
                }
              }

              if (all_valid_candidates.exists(_.isEmpty)) {
                // If any of the filtered candidates is empty, exit with failure
                Seq(None)
              } else {
                // Otherwise, recurse on each fragment
                for ((refined_fragment, valid_candidates) <- refinement_group.zip(all_valid_candidates)) yield {
                  find_representative_fragments(refined_fragment, valid_candidates)
                }
              }
            }
          }

          // Find a group where all refinements were found
          val complete_containing_fragments_group = containing_fragments_groups.find(_.forall(_ != None))

          // These fragments contain equivalent information to this fragment, return
          complete_containing_fragments_group match {
            // Failure, not all information was found
            case None =>
              None

            // Success, all refinements were already found in the fragments
            case Some(nested_fragments) =>
              Some(nested_fragments.flatten.flatten)
          }
      }
    }

    // Map fragments that need replaced with the fragments replacing them
    var fragments_to_replace = {
      (for {
        fragment <- all_fragments.par
        option_replacement_fragments = find_representative_fragments(fragment.Main, all_fragments.diff(Seq(fragment)).view.filter(candidate => candidate.Main.ObeysContract(fragment.Main)))
      } yield {
        (fragment, option_replacement_fragments)
      })
      .collect{case (fragment, Some(replacement_fragments)) => (fragment, replacement_fragments)}
    }

    println("time: " + (System.nanoTime()-time)/1e9 + "s")
    println("Fragments that need replaced: " + fragments_to_replace.size)
    println("")

    // Find instances where fragments wanted to be replaced by other fragments that were themselves going to be replaced
    for {tuple_effector <- fragments_to_replace} {
      fragments_to_replace = {
        for (tuple_receiver <- fragments_to_replace) yield {
          // Replace any fragment scheduled for deletion with the corresponding replacement fragments
          val replacement_fragments = tuple_receiver._2.map(may_replace =>
            if (may_replace == tuple_effector._1) {
              tuple_effector._2
            } else {
              Seq(may_replace)
            }
          )

          // Return
          (tuple_receiver._1, replacement_fragments.flatten)
        }
      }
    }

    // Replace fragments in the fragment clusters with equivalent fragments
    for (cluster <- fragment_clusters) {
      cluster.Fragments = {
        (for ((fragment, modifier) <- cluster.Fragments) yield {
          fragments_to_replace.find(_._1 == fragment) match {
            // No replacement found, keep this fragment
            case None =>
              Seq((fragment, modifier))

            // An set of replacement fragments was found, propagate the modifier to each member of the set
            case Some((_, replacement_fragments)) =>
              replacement_fragments.map(replacement_fragment =>
                (replacement_fragment, modifier * replacement_fragment.Main.AutomorphismCount / fragment.Main.AutomorphismCount)
              )
          }
        })
        .flatten
      }
    }

    // Update the list of all fragments
    all_fragments = fragment_clusters.flatMap(_.Fragments).map(_._1).distinct

    model_seeds = seeds
    model_observables = only_main_observables
    model_contributor_map = contributor_fragment_map
    model_fragments = all_fragments
  }

  def Save(filename: String) {
    // Ensure model is valid before saving
    if (!model_valid) {
      Validate()
    }

    val file = new FileWriter(filename)

    try {
      // Write compartments
      write_line("% Compartments" + (if (!Name.isEmpty) {" " + Name} else {""}))

      for (compartment <- Compartments) {
        write_line(compartment)
      }

      write_line()

      // Write agents
      write_line("% Agents")

      for (agent <- Agents) {
        write_line(agent)
      }

      write_line()

      // Write seeds
      write_line("% Seeds")

      for (seed <- Seeds) {
        write_line(seed)
      }

      write_line()

      // Write observables
      write_line("% Observables")
      for (observable <- Observables) {
        write_line(observable)
      }

      write_line()

      // Write parameters
      write_line("% Parameters")
      for (parameter <- Parameters) {
       write_line(parameter)
      }

      write_line()

      // Write rules
      write_line("% Rules")
      for (rule <- Rules) {
        write_line(rule)
      }
    }
    finally {
      file.close()
    }

    /********************/
    /**Helper functions**/

    def write_line(line: Any = "") {
      //#todo make this just \n
      file.write(line.toString + "\r\n")
    }
  }

  def ExportToMatlab(filename: String) {
    val file = new FileWriter(filename)

    try {
      /////
      // Write compartments
      /////
      write_line("% Compartments " + Name)

      // Write a default compartment if any fragment uses it
      if (model_fragments.map(_.Compartment).exists(_ == None)) {
        write_line("default 0 1")
      }

      // Write rest of compartments
      for (compartment <- Compartments) {
        write_line(
          compartment.Name + " " +
          compartment.Dimension + " " +
          compartment.Size
        )
      }

      write_line()

      /////
      // Write seeds
      /////
      write_line("% Seeds ")

      for (seed <- model_seeds) {
        write_line(kronecker_name(seed.Graph) + " " + seed.Value)
      }

      write_line()

      /////
      // Write fragments as species
      /////
      val species_grouped_by_compartment = model_fragments.seq.groupBy(_.Compartment) // .seq makes groupBy deterministic

      for ((compartment, species_in_compartment) <- species_grouped_by_compartment) {
        // Write species header with compartment
        compartment match {
          case Some(actual_compartment) =>
            write_line("% States " + actual_compartment.Name)

          case None =>
            write_line("% States default")
        }

        // Write each species
        for (species <- species_in_compartment) {
          val matching_seeds = {
            model_seeds.map{seed =>
              seed.Graph.MatchesCount(species.Main) match {
                case 0 => None
                case count => Some((seed, count))
              }
            }
            .flatten
          }

          val seed_string = {
            matching_seeds.map{tuple =>
              if (tuple._2 == 1) {
                kronecker_name(tuple._1.Graph)
              } else {
                kronecker_name(tuple._1.Graph) + "*" + tuple._2
              }
            }
            .mkString("+")
          }

          write_line(
            kronecker_name(species.Main)
            + (seed_string match {
              case "" => ""
              case string => " " + string
            })
          )
        }

        write_line()
      }

      /////
      // Write observables
      /////
      write_line("% Outputs")
      for (observable <- model_observables) {
        val observable_fragments = {
          for {
            contributor <- observable.Contributors
            fragment_group = model_contributor_map(contributor)
            (fragment, fragment_modifier) <- fragment_group.Fragments
          } yield {
            val modifier = contributor.Contribution * contributor.Pattern.Modifier * fragment_modifier

            // Return
            (fragment, modifier)
          }
        }

        val grouped_fragments = observable_fragments.groupBy(_._1).map(tuple => (tuple._1, tuple._2.map(_._2).sum))

        val contributor_strings = {
          // Constant
          (observable.Constant match {
            case 0.0 => Nil
            case _   => Seq(observable.Constant.toString)
          }) ++
          // Every contributor's fragments
          (for ((fragment, fragment_modifier) <- grouped_fragments) yield {
            val graph_string = "" + kronecker_output_name(fragment)

            val value_string = {
              fragment_modifier match {
                case 1.0 => ""
                case _   => "=" + fragment_modifier
              }
            }

            // Return
            graph_string + value_string
          })
        }

        write_line(observable.Name + " " + contributor_strings.mkString(" "))
      }

      write_line()

      /////
      // Write parameters
      /////
      write_line("% Parameters")
      for (parameter <- Parameters) {
        write_line(parameter.Name + " " + parameter.Value)
      }

      write_line()

      /////
      // Write rules
      /////
      write_line("% Reactions")
      for (fragment <- model_fragments) {
        // Write consumption rules
        for (rule <- fragment.ConsumptionRules) {
          rule.OtherReactant match {
            // Unimolecular consumption
            case None =>
              val matches = rule.Consumer.ConsumedCount(fragment.Main)

              val parameter_modifier = rule.RuleModifier * matches

              write_line(
                full_kronecker_name(fragment)
                + " 0 0 0 "
                + rule.Parameter.Name + modifier_string(parameter_modifier)
                + " 0"
              )

            // Bimolecular consumption
            case Some(other_reactant) =>
              for ((other_fragment, other_modifier) <- other_reactant.Fragments) {
                val matches = rule.Consumer.ConsumedCount(fragment.Main)

                val parameter_modifier = rule.RuleModifier * other_modifier * matches

                write_line(
                  full_kronecker_name(fragment)
                  + " " + full_kronecker_name(other_fragment)
                  + " " + full_kronecker_name(other_fragment)
                  + " 0 "
                  + rule.Parameter.Name + modifier_string(parameter_modifier)
                  + " 0"
                )
              }
          }
        }

        // Write production rules
        for (rule <- fragment.ProductionRules) {
          rule.OtherReactants match {
            // Zeroth-order production
            case Seq() =>
              val parameter_modifier = rule.RuleModifier

              write_line(
                "0 0 "
                + full_kronecker_name(fragment)
                + " 0 "
                + rule.Parameter.Name + modifier_string(parameter_modifier)
                + " 0"
              )

            // First-order production
            case Seq(reactant) =>
              for ((reactant_fragment, fragment_modifier) <- reactant.Fragments) {
                val parameter_modifier = rule.RuleModifier * fragment_modifier

                write_line(
                  full_kronecker_name(reactant_fragment)
                  + " 0 "
                  + full_kronecker_name(fragment)
                  + " " + full_kronecker_name(reactant_fragment)
                  + " " + rule.Parameter.Name + modifier_string(parameter_modifier)
                  + " 0"
                )
              }

            // Second-order production
            case Seq(reactant1, reactant2) =>
              for {
                (reactant1_fragment, fragment1_modifier) <- reactant1.Fragments
                (reactant2_fragment, fragment2_modifier) <- reactant2.Fragments
              } {
                val parameter_modifier = rule.RuleModifier * fragment1_modifier * fragment2_modifier

                write_line(
                  ","
                  + full_kronecker_name(reactant1_fragment)
                  + " " + full_kronecker_name(reactant2_fragment)
                  + " " + full_kronecker_name(fragment)
                  + " " + full_kronecker_name(reactant1_fragment)
                  + " " + full_kronecker_name(reactant2_fragment)
                  + " " + rule.Parameter.Name + modifier_string(parameter_modifier)
                )
              }
          }
        }

       write_line()
      }
    }
    finally {
      file.close()
    }

    /********************/
    /**Helper functions**/

    def write_line(line: Any = "") {
      //#todo make this just \n
      file.write(line.toString + "\r\n")
    }

    def kronecker_name(graph: ModelStaticGraph) = {
      graph.toString.replace(",", ";").replace(".", ":")
    }

    def kronecker_compartment(fragment: ModelFragment) = {
      fragment.Compartment match {
        case Some(c) => c.Name
        case None    => "default"
      }
    }

    def full_kronecker_name(fragment: ModelFragment) = {
      val species_name = kronecker_name(fragment.Main)

      val compartment_name = kronecker_compartment(fragment)

      // Return
      compartment_name + "." + species_name
    }

    def kronecker_output_name(fragment: ModelFragment) = {
      val full_name = full_kronecker_name(fragment)

      // Return
      ("^" + full_name + "$")
      .replace(".", """\.""")
      .replace("?", """\?""")
      .replace("{", """\{""")
      .replace("}", """\}""")
      .replace("(", """\(""")
      .replace(")", """\)""")
    }

    def modifier_string(number: Rational): String = {
      number match {
        case Rational(1,1) => ""
        case Rational(n,1) => "=" + n
        case Rational(n,d) => "=" + n + "/" + d
      }
    }
  }

}
