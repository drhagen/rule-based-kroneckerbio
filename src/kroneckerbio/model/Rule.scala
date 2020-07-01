/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model

import scala.util.control.Breaks.{break, breakable}
import kroneckerbio.parser.RuleBasedModelParser
import kroneckerbio.utilities.RegexUtility._
import kroneckerbio.utilities.Inquirable._
import kroneckerbio.implicitOption
import kroneckerbio.utilities.ErrorBuffer._

case class Rule private (Name: Option[String], Reactants: Seq[RulePattern], Products: Seq[RulePattern], ForwardParameters: Seq[RuleParameter], ReverseParameters: Seq[RuleParameter]) {
  require(Name == None || RuleBasedModelParser.name.contains(Name.get), "Rule name \"" + Name + "\" is invalid")
  require(Reactants.size <= 2 || ForwardParameters == Nil, "Too many reactants in rule; max is 2 when forward reaction exists")
  require(Products.size <= 2  || ReverseParameters == Nil, "Too many products in rule; max is 2 when reverse reaction exists")

  val IsForward = ForwardParameters != Nil
  val IsReverse = ReverseParameters != Nil

  //#TODO: do not print unambiguous numberings of monomers
  override def toString = "" +
    ((ForwardParameters, ReverseParameters) match {
      case (Nil,Nil)   => ""
      case (_::_,Nil)  => reactant_list_to_string(Reactants) + " -> "  + reactant_list_to_string(Products) + " " + parameter_list_to_string(ForwardParameters) + (Name match {case None => ""; case Some(name) => " " + name})
      case (Nil,_::_)  => reactant_list_to_string(Reactants) + " <- "  + reactant_list_to_string(Products) + " " + parameter_list_to_string(ReverseParameters) + (Name match {case None => ""; case Some(name) => " " + name})
      case (_::_,_::_) => reactant_list_to_string(Reactants) + " <-> " + reactant_list_to_string(Products) + " " + parameter_list_to_string(ForwardParameters) + " " + parameter_list_to_string(ReverseParameters) + (Name match {case None => ""; case Some(name) => " " + name})
    })

  def reactant_list_to_string(reactants: Seq[RulePattern]) = reactants match {
    case Nil => "0"
    case _ => reactants.mkString(" + ")
  }

  def parameter_list_to_string(parameters: Seq[RuleParameter]) = parameters match {
    case Nil       => "()"
    case Seq(head) => head.toString
    case list      => list.mkString("(",",",")")
  }
}

object Rule {
  // Factory constructor
  def apply(
    name: Option[String] = None,
    reactants: Option[Seq[RulePattern]] = None,
    products: Option[Seq[RulePattern]] = None,
    forward_parameters: Option[Seq[RuleParameter]] = None,
    reverse_parameters: Option[Seq[RuleParameter]] = None
  ) = {
    // Give every monomer in the reactants and products the appropriate index
    val (full_reactants, full_products) = number_monomers(reactants.getOrElse(Nil), products.getOrElse(Nil))

    // Zeroth-order rules must have fully connected products
    //#TODO: Zeroth-order rules must have only one parameter because they do not happen in specific compartments
    //#TODO: First-order rules must have only one parameter because rule compartments are equivalent to reactant compartments

    // Return
    new Rule(
      name,
      full_reactants,
      full_products,
      forward_parameters.getOrElse(Nil),
      reverse_parameters.getOrElse(Nil)
    )
  }

  /** Every monomer in the reactants is matched with a monomer in the products and vice versa, unless that monomer
   *  is created or destroyed. The user may indicate which monomer goes with which using the optional Index field
   *  of the RuleMonomer. But that is generally too much work for something that is usually unambiguous. This
   *  method give the RuleMonomers a canonical numbering according to this scheme: (1) monomers numbered by the
   *  user are left alone, (2) each monomer on the reactant side is paired with the first monomer on the product
   *  side with the same name that is also unnumbered, (3) if there are no unnumbered product monomers, the monomer
   *  is marked for destruction, (4) any product monomers that do not get numbered during this procedure get marked
   *  for production.
  */
  def number_monomers(input_reactants: Seq[RulePattern], input_products: Seq[RulePattern]) = {
    var reactants = input_reactants
    var products = input_products

    // Create a set of unused indexes to for numbering
    val number_of_reactant_monomers = reactants.map(_.Main.Monomers.size).foldLeft(0)(_+_)
    val indexes_already_used = reactants.map(_.Main).flatMap(_.Monomers).map(_.Index).flatten.distinct
    var indexes_available: Seq[Int] =(1 to number_of_reactant_monomers).diff(indexes_already_used)

    // Loop over every reactant monomer
    for (i_reactant <- 0 until reactants.size) {
      val reactant_graph_i = reactants(i_reactant).Main

      for (i_reactant_monomer <- 0 until reactant_graph_i.Monomers.size) {
        val reactant_monomer_i = reactant_graph_i.Monomers(i_reactant_monomer)

        breakable{ // WTF Scala
          // Poor man's continue:
          if (reactant_monomer_i.Index != None) {
            // The index is already assigned. Don't process this monomer.
            break()
          }

          // Search the product monomers for a match
          for (i_product <- 0 until products.size) {
            val product_graph_i = products(i_product).Main

            for (i_product_monomer <- 0 until product_graph_i.Monomers.size) {
              val product_monomer_i = product_graph_i.Monomers(i_product_monomer)

              if (reactant_monomer_i.Name == product_monomer_i.Name && product_monomer_i.Index == None) {
                // A product with the same name and no index has been found. This is the partner.

                // Find an index not used yet and remove it from the pool of available indexes
                val new_index = indexes_available(0)
                indexes_available = indexes_available.filter(_ != new_index)

                // Update reactants
                reactants = change_reactants_monomer_index(reactants, i_reactant, i_reactant_monomer, new_index)

                // Update products
                products = change_reactants_monomer_index(products, i_product, i_product_monomer, new_index)

                // Stop searching through product monomers
                break()
              }
            }
          }

          // If we got here then no matching monomer in the product side was found. This reactant monomer
          // is consumed and has an index of zero.
          reactants = change_reactants_monomer_index(reactants, i_reactant, i_reactant_monomer, 0)
        }
      }
    }

    // Find those product monomers that did not get an index, give them a zero
    for (i_product <- 0 until products.size) {
      for (i_product_monomer <- 0 until products(i_product).Main.Monomers.size) {
        if (products(i_product).Main.Monomers(i_product_monomer).Index == None) {
          products = change_reactants_monomer_index(products, i_product, i_product_monomer, 0)
        }
      }
    }

    // Return
    (reactants, products)
  }

  def change_reactants_monomer_index(
    reactants: Seq[RulePattern],
    i_reactant: Int,
    i_reactant_monomer: Int,
    new_index: Int
  ): Seq[RulePattern] = {
    reactants.updated(
      i_reactant, reactants(i_reactant).Updated(
        Main = reactants(i_reactant).Main.Updated(
          Monomers = reactants(i_reactant).Main.Monomers.updated(
            i_reactant_monomer, reactants(i_reactant).Main.Monomers(i_reactant_monomer).Updated(
              Index = Some(new_index)
            )
          )
        )
      )
    )
  }
}

case class RulePattern(Main: RuleGraph, Disjoint: Inquirable[ObservableGraph], Compartment: CompartmentSite) extends Pattern {
  def this(
    Main: RuleGraph,
    Disjoint: Option[Inquirable[ObservableGraph]] = None,
    Compartment: Option[CompartmentSite] = None
  ) = this(
    Main,
    Disjoint.getOrElse(True),
    Compartment.getOrElse(AnyCompartmentSite())
  )

  def Updated(
    Main: Option[RuleGraph] = None,
    Disjoint: Option[Inquirable[ObservableGraph]] = None,
    Compartment: Option[CompartmentSite] = None
  ) = new RulePattern(
    Main.getOrElse(this.Main),
    Disjoint.getOrElse(this.Disjoint),
    Compartment.getOrElse(this.Compartment)
  )

  override def toString = {
    "" +
    Main +
    (if (Disjoint != True) {
      ".{" + Disjoint + "}"
    }
    else {
      ""
    }) +
    (if (Compartment != AnyCompartmentSite()) {
      "." + Compartment
    }
    else {
      ""
    })
  }
}

object RulePattern {
  def apply(
    Main: RuleGraph,
    Disjoint: Option[Inquirable[ObservableGraph]] = None,
    Compartment: Option[CompartmentSite] = None
  ): RulePattern = new RulePattern(Main, Disjoint, Compartment)
}

case class RuleGraph(Monomers: Seq[RuleMonomer]) extends Graph{
  require(!Monomers.isEmpty, "A graph cannot have an empty set of monomers")
  (new ErrorBuffer("Rule graph \"" + this + "\" has unmatched edges:") ++ validate_graph_edges()).check()

  def Updated(
    Monomers: Option[Seq[RuleMonomer]] = None
  ) = new RuleGraph(
    Monomers.getOrElse(this.Monomers)
  )

  override def toString = Monomers.mkString(".")
}

case class RuleMonomer(Name: String, Sites: Seq[ObservableSite], Compartment: CompartmentSite, Index:Option[Int]) extends Monomer {
  require(RuleBasedModelParser.id.contains(Name), "Observable monomer name \"" + Name + "\" is invalid")

  def this(
    name: String,
    sites: Option[Seq[ObservableSite]] = None,
    compartment: Option[CompartmentSite] = None,
    index: Option[Int] = None
  ) = this(
    name,
    sites.getOrElse(Nil),
    compartment.getOrElse(AnyCompartmentSite()),
    index
  )

  def Updated(
    Name: Option[String] = None,
    Sites: Option[Seq[ObservableSite]] = None,
    Compartment: Option[CompartmentSite] = None,
    Index: Option[Option[Int]] = None
  ) = new RuleMonomer(
    Name.getOrElse(this.Name),
    Sites.getOrElse(this.Sites),
    Compartment.getOrElse(this.Compartment),
    Index.getOrElse(this.Index)
  )

  override def toString = {
    Name +
    (if (!Sites.isEmpty){
      Sites.mkString("(",",",")")}
    else {
      ""}) +
    Compartment +
    (Index match {
      case Some(int) => "$" + int
      case None      => ""
    })
  }
}

object RuleMonomer {
  def apply(
    name: String,
    sites: Option[Seq[ObservableSite]] = None,
    compartment: Option[CompartmentSite] = None,
    index: Option[Int] = None
  ): RuleMonomer = new RuleMonomer(name, sites, compartment, index)
}

case class RuleParameter(Name: String, Compartment: CompartmentSite) {
  require(RuleBasedModelParser.id.contains(Name), "Rule parameter name \"" + Name + "\" is invalid")

  def this(
    Name: String,
    Compartment: Option[CompartmentSite] = None
  ) = this(
      Name,
      Compartment.getOrElse(AnyCompartmentSite())
  )

  override def toString = Name + Compartment
}

object RuleParameter {
  def apply(
    Name: String,
    Compartment: Option[CompartmentSite] = None
  ): RuleParameter = new RuleParameter(Name, Compartment)
}
