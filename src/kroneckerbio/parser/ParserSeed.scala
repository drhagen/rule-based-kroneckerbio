package kroneckerbio.parser

import kroneckerbio.traversableToSeq
import kroneckerbio.model.{CompartmentName, SeedSite}

/**
 * Created with IntelliJ IDEA.
 * User: David Hagen
 * Date: 5/17/12
 * Time: 8:40 AM
 * To change this template use File | Settings | File Templates.
 */

/**
 * Seeds cannot have ambiguous compartments on their monomers. However, a model file allows the seed to inherit the
 * compartment of the seed section. A ParserSeed reflects these seeds where the compartment is optional during parsing.
 *
 * @param Graph
 * @param Value
 */
case class ParserSeed(Graph: ParserGraph, Value: Double) {
  override def toString = "" + Graph + " " + Value
}

case class ParserGraph(Monomers: List[ParserMonomer]) {
  override def toString = Monomers.mkString(".")
}

case class ParserMonomer(Name: String, Sites: Seq[SeedSite], Compartment: Option[CompartmentName]) {
  override def toString = Name + (if (!Sites.isEmpty) {Sites.mkString("(",",",")")} else {""})
}
object ParserMonomer {
  def apply(
    name: String,
    sites: Option[Traversable[SeedSite]] = None,
    compartment: Option[CompartmentName] = None
  ): ParserMonomer = new ParserMonomer(
    name,
    sites.getOrElse(Seq()),
    compartment
  )
}
