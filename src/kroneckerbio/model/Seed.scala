/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model

import scala.collection.mutable.ArrayBuffer
import kroneckerbio.implicitOption
import kroneckerbio.parser.RuleBasedModelParser
import kroneckerbio.utilities.RegexUtility._
import kroneckerbio.utilities.NegatableSeq._
import kroneckerbio.utilities.Inquirable.{True, Inquirable}
import kroneckerbio.utilities.EqualsByReference
import kroneckerbio.utilities.ErrorBuffer._

case class Seed(Graph: SeedGraph, Value: Double) {
  require(Value >= 0, "Seed value " + Value + " is invalid")

  override def toString = "" + Graph + " " + Value
}

case class SeedGraph(Monomers: Seq[SeedMonomer]) {
  require(Monomers.size > 0, "A Seed cannot have no monomers")
  (new ErrorBuffer("Seed graph \"" + this + "\" has unmatched edges:") ++ validate_graph_edges()).check()
  require(IsFullyConnected, "A Seed graph must be fully connected")

  def validate_graph_edges() = {
    // All bound edges in graph
    val bound_edges = Monomers flatMap {_.Sites} map {_.Edge} collect {case edge: BoundEdge => edge}

    // Ensure that every edge appears exactly twice
    val wrong_edge_counts = bound_edges groupBy {identity} mapValues {_.size} filter {_._2 != 2}

    // Generate errors for those that were not equal
    val errors = {
      for (edge_count <- wrong_edge_counts) yield {
        new ConcreteError("Edge #" + edge_count._1 + " appears " + edge_count._2 + " times")
      }
    }

    // Return
    errors
  }

  def IsFullyConnected: Boolean = {
    // Keep track of when all possible monomers have been found
    var last_count = 0
    var current_count = 1

    // Initialize with the first monomer
    var found_monomers = ArrayBuffer(Monomers(0))
    var next_edges = Monomers(0).Sites.map(_.Edge)

    while (last_count != current_count) {
      // Reset counter; loop will terminate if current_count does not change
      last_count = current_count

      // Look at the monomers at the other end of next edges
      val monomers_on_edges = Monomers.filter(!_.AllBoundEdges.intersect(next_edges).isEmpty)

      // Monomers not found until now
      val new_monomers = monomers_on_edges.diff(found_monomers)

      // Edges on new monomers
      next_edges = new_monomers.flatMap(_.AllBoundEdges)

      // Add newly found monomers to complete list of found monomers
      found_monomers ++= new_monomers

      // Update counter
      current_count = found_monomers.size
    }

    // Return true if all monomers were found in this way
    current_count == Monomers.size
  }

  override def toString = Monomers.mkString(".")
}

object SeedGraph {
  import SeedMonomer._

  implicit def SeedGraph2Pattern(old: SeedGraph): Pattern = new SeedPatternDuck(old, True, AnyCompartmentSite())
  implicit def SeedGraph2Graph(old: SeedGraph): Graph = new SeedGraphDuck(old.Monomers.map(SeedMonomer2Monomer(_)))
  //implicit def SeqSeedMonomer2SeqMonomer(old: Seq[SeedMonomer]): Seq[Monomer] = old.map(SeedMonomer2Monomer(_)) // Implicit Seq converters are bad!

  class SeedGraphDuck(val Monomers: Seq[Monomer]) extends Graph
  class SeedPatternDuck(val Main: Graph, val Disjoint: Inquirable[Graph], val Compartment: CompartmentSite) extends Pattern
}

case class SeedMonomer(Name: String, Sites: Seq[SeedSite] = Nil, Compartment: CompartmentName) extends EqualsByReference {
  require(RuleBasedModelParser.id.contains(Name), "Seed monomer name \"" + Name + "\" is invalid")

  val AllBoundEdges = Sites.map(_.Edge).collect{case edge: BoundEdge => edge}

  //#todo Make a toString method that accepts a compartment name and use it in the m.Save method
  override def toString = Name + (if (!Sites.isEmpty) {Sites.mkString("(",",",")")} else {""}) + "@" + Compartment
}

object SeedMonomer {
  import SeedSite._

  implicit def SeedMonomer2Monomer(old: SeedMonomer): Monomer = new SeedMonomerDuck(old.Name, old.Sites.map(SeedSite2Site(_)), TrueCompartmentSite(old.Compartment))
  //implicit def SeqSeedSite2SeqSite(old: Seq[SeedSite]): Seq[Site] = old.map((site: SeedSite) => site) // Implicit Seq converters are bad!

  case class SeedMonomerDuck(Name: String, Sites: Seq[Site], Compartment: CompartmentSite) extends Monomer
}

case class SeedSite(Name: String, State: StateName, Edge: UnambiguousEdge) {
  require(RuleBasedModelParser.id.contains(Name), "Seed monomer site name \"" + Name + "\" is invalid")

  def this(
    name: String,
    state: Option[StateName],
    edge: Option[UnambiguousEdge]
  ) = this(
    name,
    state.getOrElse(StateName("0")),
    edge.getOrElse(UnboundEdge)
  )

  override def toString =
    Name +
    (State match {
      case StateName("0") => ""
      case _              => "~" + State
    }) +
    (Edge match {
      case UnboundEdge => ""
      case _: BoundEdge   => "-" + Edge
    })
}

object SeedSite {
  implicit def SeedSite2Site(old: SeedSite): Site = new SeedSiteDuck(old.Name, TrueStateSite(old.State), EdgeSite(old.Edge))

  def apply(
    name: String,
    state: Option[StateName] = None,
    edge: Option[UnambiguousEdge] = None
  ): SeedSite = new SeedSite(name, state, edge)

  class SeedSiteDuck(val Name: String, val State: StateSite, val Edge: EdgeSite) extends Site
}
