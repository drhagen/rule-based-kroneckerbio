/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model

import kroneckerbio.parser.RuleBasedModelParser
import kroneckerbio.utilities.RegexUtility._
import kroneckerbio.utilities.RichSeq._
import kroneckerbio.utilities.ErrorBuffer._
import kroneckerbio.implicitOption

class Agent private (val Name: String, val Sites: Seq[AgentSite]) {
  require(RuleBasedModelParser.id.contains(Name), "Agent name \"" + Name + "\" is invalid")
  require(Sites.map(_.Name).isUnique, "Agent has sites with duplicate names")

  def Contains(monomer: ObservableMonomer): Boolean =
    Name == monomer.Name &&
    Sites.allExists(monomer.Sites, (agent_site, monomer_site: ObservableSite) => agent_site.Contains(monomer_site))

  def validateMonomer(monomer: Monomer) = Name match {
    case monomer.Name => new ErrorBuffer("Sites on monomer \"" + monomer + "\" does not match agent \"" + this + "\":") ++ (
      for (site <- monomer.Sites) yield (
        // Matching site on agent
        Sites.find(_.Name == site.Name)

        // Was site found?
        match {
          // No site with a matching name was found
          case None => new ConcreteError("Site \"" + site.Name + "\" does not exist")

          // Matching site was found, check states
          case Some(matchingSite) =>
            new ErrorBuffer("Monomer site \"" + site + "\" does not match agent site \"" + matchingSite + "\"") ++ (

            for (state <- site.State.AllStateNames) yield {
              new ConcreteError(
                "Monomer site state \"" + state + "\" does not exist",
                !matchingSite.States.contains(state)
              )
            })
        }
      )
    )
    case _ => new ConcreteError("Name \"" + monomer.Name + "\" does not match")
  }

  def validateUnambiguousMonomer(monomer: Monomer) = (
    ErrorBuffer("Monomer \"" + monomer.Name + "\" is not completely unambiguous:")
    ++
    (for (agent_site <- Sites) yield {
      monomer.Sites.find(_.Name == agent_site.Name) match {
        case Some(monomer_site) =>
          agent_site.validateUnambiguousSite(monomer_site)
        case None =>
          ConcreteError("Agent site \"" + agent_site.Name + "\" was not mentioned")
      }
    })
  )

  override def toString = Name + (if (!Sites.isEmpty) {Sites.mkString("(",",",")")} else {""})
}

object Agent {
  // Factory constructor
  def apply(
    Name: String,
    Sites: Option[Seq[AgentSite]] = None
  ) = new Agent(
    Name,
    Sites.getOrElse(Nil)
  )
}

class AgentSite private (val Name: String, val States: Seq[StateName]) {
  require(RuleBasedModelParser.id.contains(Name), "Agent site name \"" + Name + "\" is invalid")
  require(States.isUnique, "Agent sites has states with duplicate names")
  require(States.contains(StateName("0")), "Agent site lacks the empty state \"0\", which is required for all agent sites")

  def Contains(monomer_site: ObservableSite): Boolean = {
    Name == monomer_site.Name &&
    States.allExists(monomer_site.State.AllStateNames, (agent_state: StateName, monomer_state: StateName) => agent_state == monomer_state)
  }

  def validateUnambiguousSite(site: Site) = {
    val header = ErrorBuffer("Agent site \"" + site.Name + "\" is not completely unambiguous:")

    val state_site_is_unambiguous = {
      site.State match {
        case TrueStateSite(Seq(single)) => true
        case _ => false
      }
    }

    val edge_site_is_unambiguous = {
      site.Edge match {
        case BoundSiteEdge(_) => true
        case TrueLocalSiteEdge(Seq(UnboundEdge)) => true
        case _ => false
      }
    }

    (header
    ++ ConcreteError("State site \"" + site.State + "\" is ambiguous", !state_site_is_unambiguous)
    ++ ConcreteError("Edge site \"" + site.Edge + "\" is ambiguous", !edge_site_is_unambiguous)
    )
  }

  // Do not print the "0" state
  override def toString = Name + States.filter(_ != StateName("0")).map("~" + _).mkString("")
}

object AgentSite {
  // Factory constructor
  def apply(
    Name: String,
    States: Option[Seq[StateName]] = None
  ) = {
    var states = States.getOrElse(Seq(StateName("0")))

    if (!states.contains(StateName("0"))) {
      states = states :+ StateName("0")
    }

    // Return
    new AgentSite(Name, states)
  }
}
