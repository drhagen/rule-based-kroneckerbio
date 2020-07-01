/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model

import kroneckerbio.parser.RuleBasedModelParser
import kroneckerbio.utilities.RegexUtility._
import kroneckerbio.utilities.RichSeq

class Parameter(val Name: String, val Value: Double) {
  require(RuleBasedModelParser.id.contains(Name), "Parameter name \"" + Name + "\" is invalid")
  require(Value >= 0, println("Parameter value " + Value + " is invalid"))

  override def toString = Name + " " + Value.toString
}
