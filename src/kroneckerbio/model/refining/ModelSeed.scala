/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

import kroneckerbio.model._
import kroneckerbio.model.refining.refining._

case class ModelSeed(Graph: ModelStaticGraph, Value: Double) {
  //////
  // Methods for agent reduction
  //////
  def AllMonomerNames: Seq[ModelAgent] =
    Graph.AllStaticMonomerNames

  def AllMonomerCompartmentNames: Seq[(ModelAgent,ModelCompartment)] =
    Graph.AllStaticMonomerCompartmentNames

  def AllMonomerSiteStateNames: Seq[(ModelAgent,ModelAgentSite,ModelAgentState)] =
    Graph.AllStaticMonomerSiteStateNames

  def AllBoundEdgeSitePairNames: Seq[Set[(ModelAgent,ModelAgentSite)]] =
    Graph.AllStaticBoundEdgeSitePairNames

  override def toString = "" + Graph + " " + Value
}

object ModelSeed {
  def apply(original: Seed, compartments: Seq[ModelCompartment], agents: Seq[ModelAgent]): ModelSeed = {
    val graph = ModelStaticGraph(original.Graph, compartments, agents)
    val value = original.Value

    // Return
    ModelSeed(graph, value)
  }
}
