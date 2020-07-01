/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

import kroneckerbio.model._
import kroneckerbio.utilities.EqualsByReference
import kroneckerbio.model.refining.refining._
import kroneckerbio.utilities.Rational._

case class ModelFragmentCluster(Main: ModelStaticGraph, var Fragments: Seq[(ModelFragment, Rational)]) extends EqualsByReference {
  override def toString = Main.toString + " as " + Fragments.size
}

case class ModelFragment(
  Main: ModelStaticGraph,
  Compartment: Option[ModelCompartment],
  ConsumptionRules: Seq[ModelFragmentConsumptionRule],
  ProductionRules: Seq[ModelFragmentProductionRule]
) extends EqualsByReference {
  override def toString = Main.toString
}

case class ModelFragmentConsumptionRule(
  Parameter: Parameter,
  Consumer: ModelReactantGraph,
  RuleModifier: Rational,
  OtherReactant: Option[ModelFragmentCluster]
)

case class ModelFragmentProductionRule(
  Parameter: Parameter,
  Producer: ModelProductGraph,
  RuleModifier: Rational,
  OtherReactants: Seq[ModelFragmentCluster]
)
