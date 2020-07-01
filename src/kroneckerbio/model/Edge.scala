/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model

import kroneckerbio.parser.RuleBasedModelParser
import kroneckerbio.utilities.RegexUtility._

sealed trait Edge
object Edge {
  def apply(index: Int) = index match {
    case 0 => UnboundEdge
    case _ => BoundEdge(index)
  }
  def apply(monomer: String, site: String) = new PartialEdge(monomer, site)
}

sealed trait UnambiguousEdge extends Edge
sealed trait LocalEdge extends Edge

case object UnboundEdge extends LocalEdge with UnambiguousEdge {
  override def toString = "0"
}

// Bound edges exist as an object so that seeds can have them and to make them mirror the build edges
case class BoundEdge(Index: Int) extends UnambiguousEdge {
  require(Index > 0, println("Bound edge \"" + Index + "\" is invalid"))

  override def toString = "" + Index
}

case class PartialEdge(Monomer: String, Site: String) extends LocalEdge {
  require(RuleBasedModelParser.id.contains(Monomer), "Partial edge monomer name \"" + Monomer + "\" is invalid")
  require(RuleBasedModelParser.id.contains(Site), "Partial edge site name \"" + Site + "\" is invalid")

  override def toString = Monomer + "@" + Site
}
