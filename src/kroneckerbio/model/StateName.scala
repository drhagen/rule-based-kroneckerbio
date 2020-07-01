/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model

import kroneckerbio.parser.RuleBasedModelParser
import kroneckerbio.utilities.RegexUtility._

case class StateName(Name: String) {
  require(Name == "0" || RuleBasedModelParser.id.contains(Name), "State name \"" + Name + "\" is invalid")

  override def toString = Name
}
