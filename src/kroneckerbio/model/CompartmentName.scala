/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model

import kroneckerbio.parser.RuleBasedModelParser
import kroneckerbio.utilities.RegexUtility._

sealed trait CompartmentIdentifier

case class CompartmentDimension(Dimension: Int) extends CompartmentIdentifier {
  require(Dimension > 0, "Compartment dimension must be a positive integer")

  override def toString = "" + Dimension
}

case class CompartmentName(Name: String) extends CompartmentIdentifier {
  require(RuleBasedModelParser.id.contains(Name), "Compartment name \"" + Name + "\" is invalid")

  override def toString = Name
}
