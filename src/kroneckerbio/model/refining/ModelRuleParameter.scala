/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

import kroneckerbio.model._
import kroneckerbio.utilities.RichSeq._

case class ModelRuleParameter(Name: Parameter, Compartment: ModelStaticCompartmentSite) {
  override def toString = "" + Name.Name + Compartment
}

object ModelRuleParameter {
  def apply(original: RuleParameter, compartments: Seq[ModelCompartment], parameters: Seq[Parameter]) = {
    val parameter = parameters.FindOrDie(_.Name == original.Name)

    val compartment = ModelStaticCompartmentSite(original.Compartment, compartments)

    // Return
    new ModelRuleParameter(parameter, compartment)
  }
}
