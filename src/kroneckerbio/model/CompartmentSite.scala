/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model

import kroneckerbio.utilities.RichSeq._
import kroneckerbio.parser.RuleBasedModelParser
import kroneckerbio.utilities.RegexUtility._

// Only a subset of things allowed as rule compartments are allowed as monomer compartments
sealed trait CompartmentSite {
  def AllCompartmentNames: Seq[CompartmentName]
}

case class AnyCompartmentSite() extends CompartmentSite {
  def AllCompartmentNames = Nil

  override def toString = ""
}

case class TrueCompartmentSite(Compartments: Seq[CompartmentIdentifier]) extends CompartmentSite {
  require(Compartments.isUnique, "Monomer compartments have duplicate names")

  def this(compartment: CompartmentIdentifier) = this(Seq(compartment))

  def AllCompartmentNames = Compartments.collect{case x: CompartmentName => x}

  override def toString = {
    Compartments match {
      case Seq(state) => "@" + state
      case list       => "@" + list.mkString("{",",","}")
    }
  }
}

object TrueCompartmentSite {
  def apply(compartment: CompartmentIdentifier) = new TrueCompartmentSite(compartment)
}

case class NegatedCompartmentSite(Compartments: Seq[CompartmentIdentifier]) extends CompartmentSite {
  require(Compartments.isUnique, "Monomer compartments have duplicate names")

  def AllCompartmentNames = Compartments.collect{case x: CompartmentName => x}

  override def toString = {
    Compartments match {
      case Seq(state) => "@!" + state
      case list       => "@!" + list.mkString("{",",","}")
    }
  }
}
