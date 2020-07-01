/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.parser

import scala.util.parsing.combinator._
import kroneckerbio.model._
import kroneckerbio.implicitOption
import kroneckerbio.utilities.Inquirable._

object RuleBasedModelParser extends JavaTokenParsers {
  // Model definition is a set of blocks of Compartments, Monomers, Seeds, Observables, Parameters, and Rules

  //
  override val whiteSpace = """[ \t]+""".r

  // Standard tokens
  def id = """[a-zA-Z_]\w*""".r
  def name = """[a-zA-Z_][^\s]*""".r
  def natural_number: Parser[Int] = (
    """\d+""".r
    ^^ {case value => value.toInt}
    )
  def floating_point_number: Parser[Double] = (
    """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
    ^^ {case value => value.toDouble}
    )
  // Optional comment
  // Required newline or end of file
  // 0-inf comment/empty lines
  def endline: Parser[String] = {opt(comment) ~> (newline | eof) <~ rep(opt(comment) ~> newline) <~ opt(comment ~> eof)}
  def comment: Parser[String] = """#.*""".r
  def newline: Parser[String] = """\n|\r\n""".r
  def eof: Parser[String] = """\z""".r

  def compartment_name: Parser[CompartmentName] = (
    id
    ^^ {case compartment => CompartmentName(compartment)}
  )
  def state_name: Parser[StateName] = (
    (id | "0")
    ^^ {case state => StateName(state)}
  )

  // Model components
  //Compartment
  def compartment: Parser[Compartment] = (
    compartment_name ~ natural_number ~ floating_point_number ~ opt(compartment_name)
    ^^ {case name~dimension~size~container => Compartment(name, dimension, size, container)}
  )

  // Agent
  def agent: Parser[Agent] = (
    id ~ opt("(" ~> repsep(agent_site, ",") <~ ")")
    ^^ {case name~sites => Agent(name, sites)}
  )
  def agent_site: Parser[AgentSite] = (
    id ~ opt("~" ~> observable_state_list)
    ^^ {case name~states => AgentSite(name, states)}
  )
  def agent_state: Parser[StateName] = (
    "~" ~> state_name
    ^^ {case state => state}
  )

  // Seed
  def seed: Parser[ParserSeed] = (
    unambiguous_graph ~ floating_point_number
    ^^ {case seed~value => ParserSeed(seed, value)}
  )
  def unambiguous_graph: Parser[ParserGraph] = (
    repsep(unambiguous_monomer, ".")
    ^^ {case monomers => ParserGraph(monomers)}
  )
  def unambiguous_monomer: Parser[ParserMonomer] = (
    id ~ opt("(" ~> repsep(unambiguous_site, ",") <~ ")") ~ opt("@" ~> compartment_name)
    ^^ {case name~sites~compartment => ParserMonomer(name, sites, compartment)}
  )
  def unambiguous_site: Parser[SeedSite] = (
    id ~ opt(agent_state) ~ opt(unambiguous_edge)
    ^^ {case name~state~edge => SeedSite(name, state, edge)}
  )
  def unambiguous_edge: Parser[UnambiguousEdge] = (
    "-" ~> natural_number
    ^^ {case number => Edge(number)}
  )

  // Observable
  def observable: Parser[Observable] = (
    name ~ repsep(contributor, ",")
    ^^ {case name~contributors => Observable(name, contributors)}
  )
  def contributor: Parser[ObservableContributor] = (
    observable_pattern ~ opt("=" ~> floating_point_number)
    ^^ {case pattern~contribution => PatternContributor(pattern, contribution)}
    |
    "0" ~> opt("=" ~> floating_point_number)
    ^^ {case contribution => ConstantContributor(contribution)}
  )
  def observable_pattern: Parser[ObservablePattern] = (
    observable_graph ~ opt("." ~> "{" ~> disjoint_or <~ "}") ~ opt("." ~> observable_compartment)
    ^^ {case graph~disjoint~compartment => ObservablePattern(graph, disjoint, compartment)}
  )
  def disjoint_or: Parser[Inquirable[ObservableGraph]] = (
    repsep(disjoint_and, "|")
    ^^ {case Nil => True
        case single_term :: Nil => single_term
        case expression => Or(expression)}
  )
  def disjoint_and: Parser[Inquirable[ObservableGraph]] = (
    repsep(disjoint_xor, "&")
    ^^ {case Nil => True
        case single_term :: Nil => single_term
        case expression => And(expression)}
  )
  def disjoint_xor: Parser[Inquirable[ObservableGraph]] = (
    disjoint_term ~ opt("^" ~> disjoint_term)
    ^^ {case term~None => term
        case term1~Some(term2) => Xor(term1,term2)}
  )
  def disjoint_term: Parser[Inquirable[ObservableGraph]] = (
    ("(" ~> disjoint_or <~ ")") |
    observable_graph
    ^^ {case term => term}
  )
  def observable_graph: Parser[ObservableGraph] = (
    repsep(observable_monomer, ".")
    ^^ {case monomers => ObservableGraph(monomers)}
  )
  def observable_monomer: Parser[ObservableMonomer] = (
    id ~ opt("(" ~> repsep(observable_site, ",") <~ ")") ~ opt(observable_compartment)
    ^^ {case name~sites~compartment => ObservableMonomer(name,sites,compartment)}
  )
  def observable_site: Parser[ObservableSite] = (
    id ~ opt(observable_state) ~ opt(observable_edge)
    ^^ {case name~state~edge => ObservableSite(name,state,edge)}
  )
  def observable_state: Parser[StateSite] = (
    "~?"
    ^^ {case _ => AnyStateSite()}
    |
    "~" ~> observable_state_list
    ^^ {case list => TrueStateSite(list)}
    |
    "~!" ~> observable_state_list
    ^^ {case list => NegatedStateSite(list)}
  )
  def observable_state_list: Parser[Seq[StateName]] = (
    state_name
    ^^ {case state => Seq(state)}
    |
    "{" ~> repsep(state_name,",") <~ "}"
    ^^ {case list => list}
  )
  def observable_edge: Parser[EdgeSite] = (
    "-?"
    ^^ {case _ => AnySiteEdge()}
    |
    "-" ~> natural_number
    ^^ {case int => BoundSiteEdge(BoundEdge(int))}
    |
    "-" ~> local_observable_edge_list
    ^^ {case list => TrueLocalSiteEdge(list)}
    |
    "-!" ~> local_observable_edge_list
    ^^ {case list => NegatedLocalSiteEdge(list)}
  )
  def local_observable_edge_list: Parser[Seq[LocalEdge]] = (
    local_observable_edge
    ^^ {case item => Seq(item)}
    |
    "{" ~> repsep(local_observable_edge,",") <~ "}"
    ^^ {case list => list}
  )
  def local_observable_edge: Parser[LocalEdge] = (
    "0"
    ^^ {case _ => UnboundEdge}
    |
    (id <~ "@") ~ id
    ^^ {case monomer~site => Edge(monomer,site)}
  )
  def observable_compartment: Parser[CompartmentSite] = (
    "@?"
    ^^ {case _ => AnyCompartmentSite()}
    |
    "@" ~> observable_compartment_list
    ^^ {case list => TrueCompartmentSite(list)}
    |
    "@!" ~> observable_compartment_list
    ^^ {case list => NegatedCompartmentSite(list)}
  )
  def observable_compartment_list: Parser[Seq[CompartmentIdentifier]] = (
    compartment_identifier
    ^^ {case compartment => Seq(compartment)}
    |
    "{" ~> repsep(compartment_identifier,",") <~ "}"
    ^^ {case list => list}
  )
  def compartment_identifier: Parser[CompartmentIdentifier] = (
    compartment_name
    ^^ {case name => name}
    |
    natural_number
    ^^ {case int => CompartmentDimension(int)}
  )

  // Parameter
  def parameter: Parser[Parameter] = (
    id ~ floating_point_number
    ^^ {case name~value => new Parameter(name, value)}
  )

  // Rule
  def rule: Parser[Rule] = (
    reactants ~ "->" ~ products  ~ parameters ~ opt(name)
    ^^ {case reactants~"->"~products~parameters~name => Rule(name,reactants,products,parameters)}
    |
    reactants ~ "<->" ~ reactants ~ parameters ~ parameters ~ opt(name)
    ^^ {case reactants~"<->"~products~forward~reverse~name => Rule(name,reactants,products,forward,Some(reverse))} // Scala bug requires a Some() here
    |
    products ~ "<-"  ~ reactants ~ parameters ~ opt(name)
    ^^ {case products~"<-"~reactants~parameters~name => Rule(name,reactants,products,parameters)}
  )
  def reactants: Parser[List[RulePattern]] = (
    "0"
    ^^ {case "0" => List[RulePattern]()}
    |
    rule_pattern ~ opt("+" ~> rule_pattern)
    ^^ {
      case reactant~None             => List(reactant)
      case reactant1~Some(reactant2) => List(reactant1,reactant2)
    }
  )
  def products: Parser[List[RulePattern]] = (
    "0"
    ^^ {case _ => Nil}
    |
    rep1sep(rule_pattern, "+")
    ^^ {case x => x}
  )
  def parameters: Parser[List[RuleParameter]] = (
    "(" ~> rep1sep(rule_parameter, ",") <~ ")"
    ^^ {case list => list}
    |
    rule_parameter
    ^^ {case parameter => List(parameter)}
  )
  def rule_parameter: Parser[RuleParameter] = (
    id ~ opt(observable_compartment)
    ^^ {case name~compartment => RuleParameter(name, compartment)}
  )
  def rule_pattern: Parser[RulePattern] = (
    rule_graph ~ opt("." ~> "{" ~> disjoint_or <~ "}") ~ opt("." ~> observable_compartment)
    ^^ {case graph~disjoint~compartment => RulePattern(graph, disjoint, compartment)}
  )
  def rule_graph: Parser[RuleGraph] = (
    rep1sep(rule_monomer, ".")
    ^^ {case monomers => RuleGraph(monomers)}
  )
  def rule_monomer: Parser[RuleMonomer] = (
    id ~ opt("(" ~> repsep(observable_site, ",") <~ ")") ~ opt(observable_compartment) ~ opt("$" ~> natural_number)
    ^^ {case name~sites~compartment~index => RuleMonomer(name, sites, compartment, index)}
  )

  // Model sections
  def compartment_section: Parser[CompartmentSection] = (
    ("%" ~> """(?i)compartment(s)?""".r ~> opt(name) <~ endline) ~
    rep(compartment <~ endline) ^^ {
      case name~lines => CompartmentSection(lines, name)}
  )
  def agent_section: Parser[AgentSection] = (
    ("%" ~> """(?i)agent(s)?""".r ~> endline) ~>
    rep(agent <~ endline) ^^ {
      case lines => AgentSection(lines)}
  )
  def seed_section: Parser[SeedSection] = (
    ("%" ~> """(?i)seed(s)?""".r ~> opt(compartment_name) <~ endline) ~
    rep(seed <~ endline) ^^ {
      case compartment~lines => SeedSection(lines, compartment)}
  )
  def observable_section: Parser[ObservableSection] = (
    ("%" ~> """(?i)observable(s)?""".r ~> opt(observable_compartment) <~ endline) ~
    rep(observable <~ endline) ^^ {
      case compartment~lines => ObservableSection(lines, compartment)}
  )
  def parameter_section: Parser[ParameterSection] = (
    ("%" ~> """(?i)parameter(s)?""".r ~> endline) ~>
    rep(parameter <~ endline) ^^ {
      case lines => ParameterSection(lines)}
  )
  def rule_section: Parser[RuleSection] = (
    ("%" ~> """(?i)rule(s)?""".r ~> endline) ~>
    rep(rule <~ endline) ^^ {
      case lines => RuleSection(lines)}
  )

  // Entire file
  def model_sections: Parser[List[ModelSection]] = (
    """([ \t]*(#.*)?(\n|\r\n|\z))*""".r ~>
    rep(compartment_section | agent_section | seed_section | observable_section | parameter_section | rule_section) ^^ {
      case list => list}
  )
}
