/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

import kroneckerbio.model._
import kroneckerbio.utilities.EqualsByReference
import kroneckerbio.utilities.RichSeq._
import kroneckerbio.model.refining.refining._

case class ModelAgent(Name: String, var Sites: Seq[ModelAgentSite], var Compartments: Seq[ModelCompartment]) extends EqualsByReference {
  def FindSite(name: String) = Sites.FindOrDie(_.Name == name)

  override def toString = {
    Name + Sites.mkString("(",",",")") + "@{" + Compartments.map(_.Name).mkString(",") + "}"
  }
}

object ModelAgent {
  def apply(original: Agent, compartments: Seq[ModelCompartment]): ModelAgent = {
    // Create a new agent
    val agent = ModelAgent(original.Name, Nil, compartments)

    // Add each site to it
    agent.Sites = original.Sites.map(site => ModelAgentSite(site, agent))

    // Return
    agent
  }

  // Turn the list of agents into an initial list of agents; all compartments, all states, and all edges are
  // considered possible until the seeds and rules are analysed later
  def BuildAgents(original_agents: Seq[Agent], compartments: Seq[ModelCompartment]): Seq[ModelAgent] = {
    val initial_agents = original_agents.map(ModelAgent(_, compartments))

    for {
      receiving_agent <- initial_agents
      receiving_site <- receiving_agent.Sites
      giving_agent <- initial_agents
    } {
      // Add every site to every other site as a possible binding partner
      receiving_site.Edges ++= giving_agent.Sites
    }

    // Return
    initial_agents
  }

  def ReduceAgents(
    agents: Seq[ModelAgent],
    build_seeds: Seq[ModelSeed],
    build_rules: Seq[ModelRule]
  ) {
    // Names of all monomers in seeds or created
    val seed_monomers = build_seeds.flatMap(_.AllMonomerNames)
    val rule_monomers = build_rules.flatMap(_.AllProducedMonomers)
    val existing_monomers = (seed_monomers ++ rule_monomers).distinct

    // Tuples of monomer-compartments in seeds or rules
    val seed_compartments = build_seeds.flatMap(_.AllMonomerCompartmentNames)
    val rule_compartments = build_rules.flatMap(_.AllProducedMonomerCompartmentNames)
    val existing_compartments = (seed_compartments ++ rule_compartments).distinct

    // Tuples of all monomer-site-states in seeds or created
    val seed_states = build_seeds.flatMap(_.AllMonomerSiteStateNames)
    val rule_states = build_rules.flatMap(_.AllProducedMonomerSiteStateNames)
    val existing_states = (seed_states ++ rule_states).distinct

    // Tuples of all monomer-site-monomer-site pairs in seeds or created
    val seed_edges = build_seeds.flatMap(_.AllBoundEdgeSitePairNames)
    val rule_edges = build_rules.flatMap(_.AllProducedBoundEdgeSitePairNames)
    val existing_bounds: Seq[Set[(ModelAgent,ModelAgentSite)]] = (seed_edges ++ rule_edges).distinct

    // Reduce compartments to those that exist
    for (agent <- agents) {
      // Compartments
      val agent_compartments = {
        existing_compartments.collect{
          case (`agent`, agent_compartment) => agent_compartment
        }
      }

/*
      val compartments_with_same_dimension = {
        //#TODO: Make this heuristic for which compartments are possible more efficient
        (for (agent_compartment <- agent_compartments) yield {
          // Get the dimension of each allowed compartment
          val compartment_dim = agent_compartment.Dimension

          // Return only the compartments with this same dimension
          agent.Compartments.filter(_.Dimension == compartment_dim)
        })
        // Flatten all results and keep only the distinct names
        .flatten
        .distinct
      }
*/

      // Attach compartments to agent
      agent.Compartments = agent_compartments
    }

    // Reduce states and edges on sites to those that exist
    for {
      agent <- agents
      site <- agent.Sites
    } {
      // Reduce states
      val states_that_go_here = existing_states.filter(
        tuple => (tuple._1 == agent && tuple._2 == site)
      )

      site.States = site.States.intersect(states_that_go_here.map(_._3))

      // Reduce edges
      val sites_that_connect_here = {
        existing_bounds
        // Keep any set that has the current site as a member
        .collect{
          // Single element set means that site connects to itself
          case set if set == Set((agent, site)) => site
          // Double element set means that this site connects to another
          case set if set.contains((agent, site)) => (set - Tuple2(agent, site)).head._2
        }
      }

      site.Edges = site.Edges.intersect(sites_that_connect_here)
    }
  }
}

case class ModelAgentSite(
  Parent: ModelAgent,
  Name: String,
  var States: Seq[ModelAgentState],
  var Edges: Seq[ModelAgentSite]
) extends EqualsByReference {

  var StateSitesAdjacentToStateSites = Map[ModelAgentState,Map[ModelAgentSite,Seq[ModelAgentState]]]()

  var EdgeSitesAdjacentToStateSites = Map[ModelAgentState,Map[ModelAgentSite,Seq[ModelLocalEdge]]]()

  var StateSitesAdjacentToEdgeSites = Map[ModelLocalEdge,Map[ModelAgentSite,Seq[ModelAgentState]]]()

  var EdgeSitesAdjacentToEdgeSites = Map[ModelLocalEdge,Map[ModelAgentSite,Seq[ModelLocalEdge]]]()

  private val attached_site_names = Edges.map(site => (site.Parent.Name, site.Name))

  def SetAdjacentSites(seeds: Seq[ModelSeed], rules: Seq[ModelRule]) {
    StateSitesAdjacentToStateSites = {
      (for (state <- States) yield {
        val other_sites = {
          (for {
            other_agent_site <- Parent.Sites
            if other_agent_site != this
          } yield {
            val state_sites_from_seeds = {
              for {
                seed <- seeds
                monomer <- seed.Graph.Monomers
                if monomer.Agent == Parent
                // This monomer currently contains the state we are considering
                this_site = monomer.StateSites.FindOrDie(_.AgentSite == this)
                if this_site.Static.contains(state)
                // Fetch the state on the other site we are considering
                other_site = monomer.StateSites.FindOrDie(_.AgentSite == other_agent_site)
              } yield {
                other_site.Static(0) // There can only be one
              }
            }

            val state_sites_from_rules = {
              (for {
                rule <- rules
                monomer <- rule.Core.Monomers.collect{case x if x.IsModelProductMonomer => x.asInstanceOf[ModelProductMonomer]}
                if monomer.Agent == Parent
                // This monomer currently contains the state we are considering
                this_site = monomer.StateSites.FindOrDie(_.AgentSite == this)
                if this_site.Resulting.contains(state)
                // Fetch the state on the other site we are considering
                other_site = monomer.StateSites.FindOrDie(_.AgentSite == other_agent_site)
                if this_site.isInstanceOf[ModelProducingStateSite] || other_site.isInstanceOf[ModelProducingStateSite]
              } yield {
                other_site.Resulting
              })
              .flatten
            }

            val state_sites = (state_sites_from_seeds ++ state_sites_from_rules).distinct

            // Return
            (other_agent_site, state_sites)
          }).toMap
        }

        // Return
        (state, other_sites)
      }).toMap
    }

    EdgeSitesAdjacentToStateSites = {
      (for (state <- States) yield {
        val other_sites = {
          (for {
            other_agent_site <- Parent.Sites
          } yield {
            val edge_sites_from_seeds = {
              for {
                seed <- seeds
                monomer <- seed.Graph.Monomers
                if monomer.Agent == Parent
                // This monomer currently contains the state we are considering
                this_site = monomer.StateSites.FindOrDie(_.AgentSite == this)
                if this_site.Static.contains(state)
                // Fetch the state on the other site we are considering
                other_site = monomer.EdgeSites.FindOrDie(_.AgentSite == other_agent_site)
              } yield {
                other_site.Resulting(0) // There can only be one
              }
            }

            val edge_sites_from_rules = {
              (for {
                rule <- rules
                monomer <- rule.Core.Monomers.collect{case x if x.IsModelProductMonomer => x.asInstanceOf[ModelProductMonomer]}
                if monomer.Agent == Parent
                // This monomer currently contains the state we are considering
                this_site = monomer.StateSites.FindOrDie(_.AgentSite == this)
                if this_site.Resulting.contains(state)
                // Fetch the state on the other site we are considering
                other_site = monomer.EdgeSites.FindOrDie(_.AgentSite == other_agent_site)
                if this_site.isInstanceOf[ModelProducingStateSite] || other_site.isInstanceOf[ModelProducingEdgeSite]
              } yield {
                other_site.Resulting
              })
              .flatten
            }

            val edge_sites = (edge_sites_from_seeds ++ edge_sites_from_rules).distinct

            // Return
            (other_agent_site, edge_sites)
          }).toMap
        }

        // Return
        (state, other_sites)
      }).toMap
    }

    StateSitesAdjacentToEdgeSites = {
      (for (edge <- Edges.map(ModelPartialEdge(_)) :+ ModelUnboundEdge) yield {
        val other_sites = {
          (for {
            other_agent_site <- Parent.Sites
          } yield {
            val state_sites_from_seeds = {
              for {
                seed <- seeds
                monomer <- seed.Graph.Monomers
                if monomer.Agent == Parent
                // This monomer currently contains the state we are considering
                this_site = monomer.EdgeSites.FindOrDie(_.AgentSite == this)
                if this_site.Resulting.contains(edge)
                // Fetch the state on the other site we are considering
                other_site = monomer.StateSites.FindOrDie(_.AgentSite == other_agent_site)
              } yield {
                other_site.Static(0) // There can only be one
              }
            }

            val state_sites_from_rules = {
              (for {
                rule <- rules
                monomer <- rule.Core.Monomers.collect{case x if x.IsModelProductMonomer => x.asInstanceOf[ModelProductMonomer]}
                if monomer.Agent == Parent
                // This monomer currently contains the state we are considering
                this_site = monomer.EdgeSites.FindOrDie(_.AgentSite == this)
                if this_site.Resulting.contains(edge)
                // Fetch the state on the other site we are considering
                other_site = monomer.StateSites.FindOrDie(_.AgentSite == other_agent_site)
                if this_site.isInstanceOf[ModelProducingEdgeSite] || other_site.isInstanceOf[ModelProducingStateSite]
              } yield {
                other_site.Resulting
              })
              .flatten
            }

            val state_sites = (state_sites_from_seeds ++ state_sites_from_rules).distinct

            // Return
            (other_agent_site, state_sites)
          }).toMap
        }

        // Return
        (edge, other_sites)
      }).toMap
    }

    EdgeSitesAdjacentToEdgeSites = {
      (for (edge <- Edges.map(ModelPartialEdge(_)) :+ ModelUnboundEdge) yield {
        val other_sites = {
          (for {
            other_agent_site <- Parent.Sites
            if other_agent_site != this
          } yield {
            val edge_sites_from_seeds = {
              for {
                seed <- seeds
                monomer <- seed.Graph.Monomers
                if monomer.Agent == Parent
                // This monomer currently contains the state we are considering
                this_site = monomer.EdgeSites.FindOrDie(_.AgentSite == this)
                if this_site.Resulting.contains(edge)
                // Fetch the state on the other site we are considering
                other_site = monomer.EdgeSites.FindOrDie(_.AgentSite == other_agent_site)
              } yield {
                other_site.Resulting(0) // There can only be one
              }
            }

            val edge_sites_from_rules = {
              (for {
                rule <- rules
                monomer <- rule.Core.Monomers.collect{case x if x.IsModelProductMonomer => x.asInstanceOf[ModelProductMonomer]}
                if monomer.Agent == Parent
                // This monomer currently contains the state we are considering
                this_site = monomer.EdgeSites.FindOrDie(_.AgentSite == this)
                if this_site.Resulting.contains(edge)
                // Fetch the state on the other site we are considering
                other_site = monomer.EdgeSites.FindOrDie(_.AgentSite == other_agent_site)
                if this_site.isInstanceOf[ModelProducingEdgeSite] || other_site.isInstanceOf[ModelProducingEdgeSite]
              } yield {
                other_site.Resulting
              })
              .flatten
            }

            val edge_sites = (edge_sites_from_seeds ++ edge_sites_from_rules).distinct

            // Return
            (other_agent_site, edge_sites)
          }).toMap
        }

        // Return
        (edge, other_sites)
      }).toMap
    }
  }

  def FindSiteState(name: StateName) = States.FindOrDie(_.Name == name)

  def IsAttached(monomer: String, site: String) = attached_site_names.contains((monomer, site))

  def EntryReachableSites = {
    var sites_found = Seq(this)
    var new_sites = Seq(this) // Process these newly found sites to find more sites

    while (new_sites.size > 0) {
      // Find next sites
      new_sites = new_sites.flatMap(_.NextEntrySites).diff(sites_found)

      // Add them to the list of sites found
      sites_found ++= new_sites
    }

    // Return
    sites_found
  }

  def EntryReachableMonomers = EntryReachableSites.map(_.Parent).distinct

  def ExitReachableSites = (Seq(this) ++ Edges.flatMap(_.EntryReachableSites)).distinct

  def ExitReachableMonomers = Edges.flatMap(_.EntryReachableMonomers).distinct

  def SitesOnSameMonomer = Parent.Sites.diff(Seq(this))

  def NextEntrySites = SitesOnSameMonomer.flatMap(_.Edges)

  override def toString = {
    Name +
    "~{" + States.mkString(",") + "}" +
    "-{0" + Edges.map(edge => "," + edge.Parent.Name + "@" + edge.Name).mkString("") + "}"
  }

}

object ModelAgentSite {
  def apply(original: AgentSite, parent: ModelAgent): ModelAgentSite = {
    // Create new site
    val site = ModelAgentSite(parent, original.Name, Nil, Nil)

    // Add each state to it
    site.States = original.States.map(state => ModelAgentState(site, state))

    // Return
    site
  }
}

case class ModelAgentState(Parent: ModelAgentSite, Name: StateName) extends EqualsByReference {

  override def toString = "" + Name
}
