/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model

import kroneckerbio.implicitOption
import kroneckerbio.utilities.NegatableSeq._
import kroneckerbio.parser.RuleBasedModelParser
import kroneckerbio.utilities.RegexUtility._
import kroneckerbio.utilities.RichSeq._
import kroneckerbio.utilities.Inquirable._
import kroneckerbio.utilities.ErrorBuffer.ErrorBuffer

case class Observable(Name: String, Contributors: Seq[ObservableContributor]) {
  require(RuleBasedModelParser.name.contains(Name), "Observable name \"" + Name + "\" is invalid")

  // Return only the true patterns in the observable
  def PatternsOnly = Contributors collect {case x: PatternContributor => x}

  // Return only the constant patterns
  def ConstantsOnly = Contributors collect {case x: ConstantContributor => x}

  override def toString = Name + " " + Contributors.mkString(" ")
}

sealed trait ObservableContributor{def Contribution: Double}

case class ConstantContributor(Contribution: Double) extends ObservableContributor {
  def this(contribution: Option[Double] = None) =
    this(contribution.getOrElse(0.0))

  override def toString = "0" + (if (Contribution != 0.0) {"=" + Contribution} else {""})
}
object ConstantContributor {
  def apply(
    contribution: Option[Double] = None
  ): ConstantContributor = new ConstantContributor(
    contribution.getOrElse(0.0)
  )
}

case class PatternContributor(Pattern: ObservablePattern, Contribution: Double) extends ObservableContributor {
  def copy(
    Pattern: Option[ObservablePattern] = None,
    Contribution: Option[Double] = None
  ) = PatternContributor(
    Pattern.getOrElse(this.Pattern),
    Contribution.getOrElse(this.Contribution)
  )

  override def toString = "" + Pattern + (if (Contribution != 1.0) {"=" + Contribution} else {""})
}
object PatternContributor {
  def apply(
    Pattern: ObservablePattern,
    Contribution: Option[Double] = None
  ): PatternContributor = new PatternContributor(
    Pattern,
    Contribution.getOrElse(1.0)
  )
}

case class ObservablePattern(Main: ObservableGraph, Disjoint: Inquirable[ObservableGraph], Compartment: CompartmentSite) extends Pattern {
  //validate_pattern_edges().check()
  def this(
    Main: ObservableGraph,
    Disjoint: Option[Inquirable[ObservableGraph]] = None,
    Compartment: Option[CompartmentSite] = None
  ) = this(
    Main,
    Disjoint.getOrElse(True),
    Compartment.getOrElse(AnyCompartmentSite())
  )

  def Updated(
    Main: Option[ObservableGraph] = None,
    Disjoint: Option[Inquirable[ObservableGraph]] = None,
    Compartment: Option[CompartmentSite] = None
  ): ObservablePattern = new ObservablePattern(
    Main.getOrElse(this.Main),
    Disjoint.getOrElse(this.Disjoint),
    Compartment.getOrElse(this.Compartment)
  )

  override def toString = {"" +
    Main +
    (if (Disjoint != True) {
      ".{" + Disjoint + "}"
    }
    else {
      ""
    }) +
    Compartment
  }
}

object ObservablePattern {
  def apply(
    Main: ObservableGraph,
    Disjoint: Option[Inquirable[ObservableGraph]] = None,
    Compartment: Option[CompartmentSite] = None
  ): ObservablePattern = new ObservablePattern(Main, Disjoint, Compartment)

  implicit def observableGraph2ObservablePattern(graph: ObservableGraph) = ObservablePattern(graph)
}

case class ObservableGraph(Monomers: Seq[ObservableMonomer]) extends Graph {
  require(Monomers != Nil, "A graph cannot have an empty list of monomers")
  (new ErrorBuffer("Observable graph \"" + this + "\" has unmatched edges") ++ validate_graph_edges()).check()

  override def toString = Monomers.mkString(".")
}

case class ObservableMonomer(Name: String, Sites: Seq[ObservableSite], Compartment: CompartmentSite) extends Monomer {
  require(RuleBasedModelParser.id.contains(Name), "Observable monomer name \"" + Name + "\" is invalid")
  require(Sites.map(_.Name).isUnique, "Monomer sites have duplicate names")

  def this(
    name: String,
    sites: Option[List[ObservableSite]] = None,
    compartment: Option[CompartmentSite] = None
  ) = this(
    name,
    sites.getOrElse(Nil),
    compartment.getOrElse(AnyCompartmentSite())
  )

  def Updated(
    name: Option[String] = None,
    sites: Option[Seq[ObservableSite]] = None,
    compartment: Option[CompartmentSite] = None
  ) = new ObservableMonomer(
    name.getOrElse(this.Name),
    sites.getOrElse(this.Sites),
    compartment.getOrElse(this.Compartment)
  )

  override def toString =
    Name +
    (if (!Sites.isEmpty){
      Sites.mkString("(",",",")")}
    else {
      ""}) +
    Compartment
}

object ObservableMonomer {
  def apply(
    name: String,
    sites: Option[List[ObservableSite]] = None,
    compartment: Option[CompartmentSite] = None
  ): ObservableMonomer = new ObservableMonomer(name, sites, compartment)
}

case class ObservableSite(Name: String, State: StateSite, Edge: EdgeSite) extends Site {
  require(RuleBasedModelParser.id.contains(Name), "Observable monomer site name \"" + Name + "\" is invalid")

  def this(
    Name: String,
    State: Option[StateSite] = None,
    Edge: Option[EdgeSite] = None
  ) = this(
    Name,
    State.getOrElse(TrueStateSite(Seq(StateName("0")))),
    Edge.getOrElse(TrueLocalSiteEdge(Seq(UnboundEdge)))
  )

  def Updated(
    Name: Option[String] = None,
    State: Option[StateSite] = None,
    Edge: Option[EdgeSite] = None
  ) = new ObservableSite(
    Name.getOrElse(this.Name),
    State.getOrElse(this.State),
    Edge.getOrElse(this.Edge)
  )

  override def toString = Name + State + Edge
}

object ObservableSite {
  def apply(
      Name: String,
      State: Option[StateSite] = None,
      Edge: Option[EdgeSite] = None
  ): ObservableSite = new ObservableSite(Name, State, Edge)
}
