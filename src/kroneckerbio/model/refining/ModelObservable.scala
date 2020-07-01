/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

import kroneckerbio.model._
import kroneckerbio.model.refining.refining._

case class ModelObservable(Name: String, Constant: Double, Contributors: Seq[ModelContributor]) {
  def Reduced(compartments: Seq[ModelCompartment]) =
    copy(Contributors = Contributors.map(_.Reduced))

  def PatternCompartmentsRemoved(compartments: Seq[ModelCompartment]): ModelObservable =
    copy(Contributors = Contributors.map(_.PatternCompartmentsRemoved(compartments)))

  def DisjointRemoved: ModelObservable = {
    val refined_contributors = Contributors.map(_.DisjointRemoved).flatten

    copy(Contributors = refined_contributors)
  }

  def RefineContributorsTowardReactant(reactant: ModelReactantGraph): ModelObservable = {
    this.copy(Contributors = Contributors.map(_.RefinePatternTowardReactant(reactant)).flatten)
  }

  override def toString = {
    Name + " " +
    Constant +
    (if (Contributors.isEmpty) {
      ""
    }
    else {
      "," +
      Contributors.mkString(",")
    })
  }
}

object ModelObservable {
  def apply(
    original: Observable,
    compartments: Seq[ModelCompartment],
    agents: Seq[ModelAgent]
  ): ModelObservable = {
    val name = original.Name

    val constant = original.Contributors.collect{case old: ConstantContributor => old}.map{_.Contribution}.sum

    val contributors =
      original.Contributors
      .collect{
        case original_contributor: PatternContributor => ModelContributor(original_contributor, compartments, agents)
      }

    // Return
    ModelObservable(name, constant, contributors)
  }
}

case class ModelContributor(Pattern: ModelObservedPattern, Contribution: Double) {
  def Reduced =
    copy(Pattern = Pattern.Reduced())

  def PatternCompartmentsRemoved(compartments: Seq[ModelCompartment]): ModelContributor =
    copy(Pattern = Pattern.PatternCompartmentsRemoved(compartments))

  def DisjointRemoved: Seq[ModelContributor] = {
    val refined_patterns = Pattern.DisjointRemoved

    for (pattern <- refined_patterns) yield {
      copy(Pattern = pattern)
    }
  }

  def RefinePatternTowardReactant(reactant: ModelReactantGraph): Seq[ModelContributor] = {
    val refined_graphs = Pattern.Main.RefineConsumedAroundReactant(reactant)

    for (graph <- refined_graphs) yield {
      copy(Pattern = Pattern.copy(Main = graph))
    }
  }

  override def toString = {
    "" +
    Pattern +
    (if (Contribution == 1.0) {
      ""
    }
    else {
      "=" + Contribution
    })
  }
}

object ModelContributor {
  def apply(
    original: PatternContributor,
    compartments: Seq[ModelCompartment],
    agents: Seq[ModelAgent]
  ): ModelContributor = {
    val pattern = RefineObservedPattern(original.Pattern, compartments, agents)
    val contribution = original.Contribution

    // Return
    ModelContributor(pattern, contribution)
  }
}
