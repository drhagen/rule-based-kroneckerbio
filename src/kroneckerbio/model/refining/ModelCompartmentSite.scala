/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

import kroneckerbio.model._
import kroneckerbio.utilities.EqualsByReference
import kroneckerbio.utilities.RichSeq._

sealed trait ModelCompartmentSite extends EqualsByReference {
  type ThisType <: ModelCompartmentSite

  def Copy(): ThisType

  def Reduced(compartments: Seq[ModelCompartment]): ThisType
}

///////////////////
//// Factories ////
///////////////////
//#TODO: Make the produced compartments do something with the the possible compartments

object ModelCompartmentSite {
  def resolve_compartments(original: CompartmentSite, compartments: Seq[ModelCompartment]) = {
    original match {
      case AnyCompartmentSite() => compartments
      case TrueCompartmentSite(identifiers) => {
        compartments.intersect(resolve_compartment_identifiers(identifiers, compartments))
      }
      case NegatedCompartmentSite(identifiers) => {
        compartments.diff(resolve_compartment_identifiers(identifiers, compartments))
      }
    }
  }

  def resolve_compartment_identifiers(original_identifiers: Seq[CompartmentIdentifier], compartments: Seq[ModelCompartment]) = {
    original_identifiers.flatMap{
      case CompartmentDimension(dimension) => compartments.filter(_.Dimension == dimension)
      case compartment: CompartmentName    => Seq(compartments.FindOrDie(_.Name == compartment))
    }
  }
}

object ModelConsumedCompartmentSite {
  import ModelCompartmentSite._

  def apply(original: CompartmentSite, compartments: Seq[ModelCompartment]): ModelConsumedCompartmentSite =
    ModelConsumedCompartmentSite(resolve_compartments(original, compartments))
}

object ModelProducedCompartmentSite {
  import ModelCompartmentSite._

  def apply(original: CompartmentSite, compartments: Seq[ModelCompartment]): ModelProducedCompartmentSite = {
    // Must only be one
    ModelProducedCompartmentSite(resolve_compartments(original, compartments)(0))
  }
}

object ModelStaticCompartmentSite {
  import ModelCompartmentSite._

  def apply(original: CompartmentSite, compartments: Seq[ModelCompartment]): ModelStaticCompartmentSite =
    ModelStaticCompartmentSite(resolve_compartments(original, compartments))
}


////////////////
//// Traits ////
////////////////

// Location
sealed trait ModelReactantCompartmentSite
  extends ModelCompartmentSite {
  type ThisType <: ModelReactantCompartmentSite

  def Observed: Seq[ModelCompartment]
  def ToStaticReactant: ModelStaticCompartmentSite

  def Copy(Observed: Seq[ModelCompartment]): ThisType

  def Reduced(compartments: Seq[ModelCompartment]): ThisType =
    Copy(Observed = Observed.intersect(compartments))

  // Does this match the supplied contract
  def ObeysContract(contract: ModelStaticCompartmentSite): Boolean = {
    Observed.forall(contract.Observed.contains(_))
  }

  def DisobeysContract(contract: ModelStaticCompartmentSite): Boolean = {
    Observed.intersect(contract.Observed).isEmpty
  }

  // Refine the compartment
  def RefineToward(refiner: ModelStaticCompartmentSite): ThisType = {
    Copy(Observed = Observed.intersect(refiner.Observed))
  }

  def RefineAgainst(refiner: ModelStaticCompartmentSite): ThisType = {
    Copy(Observed = Observed.diff(refiner.Observed))
  }
}

sealed trait ModelProductCompartmentSite
  extends ModelCompartmentSite {
  type ThisType <: ModelProductCompartmentSite

  def ToStaticProduct: ModelStaticCompartmentSite
}

sealed trait ModelConservedCompartmentSite
  extends ModelReactantCompartmentSite
  with ModelProductCompartmentSite {
  type ThisType <: ModelConservedCompartmentSite
}

// Structure
sealed trait ModelConsumingCompartmentSite
  extends ModelReactantCompartmentSite {
  type ThisType <: ModelConsumingCompartmentSite

  def Consumed: Seq[ModelCompartment]
}

sealed trait ModelProducingCompartmentSite
  extends ModelProductCompartmentSite {
  type ThisType <: ModelProducingCompartmentSite

  def Produced: ModelCompartment
}

/////////////////
//// Classes ////
/////////////////

final case class ModelConsumedCompartmentSite(Consumed: Seq[ModelCompartment]) extends ModelConsumingCompartmentSite {
  type ThisType = ModelConsumedCompartmentSite

  def Observed = Consumed
  def ToStaticReactant = ModelStaticCompartmentSite(Consumed)

  def Copy() = copy()
  def Copy(Observed: Seq[ModelCompartment]) = copy(Consumed = Observed)

  override def toString = "@" + Consumed.map(_.Name).mkString("{",",","}")
}

final case class ModelProducedCompartmentSite(Produced: ModelCompartment) extends ModelProducingCompartmentSite {
  type ThisType = ModelProducedCompartmentSite

  def Reduced(compartments: Seq[ModelCompartment]) = this

  def ToStaticProduct = ModelStaticCompartmentSite(Seq(Produced))

  def Copy() = copy()

  override def toString = "@{" + Produced.Name + "}"
}

final case class ModelChangedCompartmentSite(
  Consumed: Seq[ModelCompartment],
  Produced: ModelCompartment
) extends ModelConservedCompartmentSite
with ModelProducingCompartmentSite
with ModelConsumingCompartmentSite {
  type ThisType = ModelChangedCompartmentSite

  def Observed = Consumed
  def ToStaticReactant = ModelStaticCompartmentSite(Consumed)
  def ToStaticProduct = ModelStaticCompartmentSite(Seq(Produced))

  def Copy() = copy()
  def Copy(Observed: Seq[ModelCompartment]) = copy(Observed)

  override def toString = "@" + (Consumed :+ Produced).map(_.Name).mkString("{",",","}")
}

final case class ModelStaticCompartmentSite(Static: Seq[ModelCompartment])
  extends ModelConservedCompartmentSite {
  type ThisType = ModelStaticCompartmentSite

  def Observed = Static
  def ToStaticReactant = this
  def ToStaticProduct = this

  def Copy() = copy()
  def Copy(Observed: Seq[ModelCompartment]) = copy(Observed)

  def IsConsumedBy(reactant_compartment: ModelReactantCompartmentSite): Boolean = {
    reactant_compartment match {
      case ModelConsumedCompartmentSite(consumed) =>
        !consumed.intersect(Observed).isEmpty

      case ModelChangedCompartmentSite(consumed, produced) =>
        !Observed.contains(produced) && !consumed.intersect(Observed).isEmpty

      case _: ModelStaticCompartmentSite =>
        false
    }
  }

  def IsProducedBy(reactant_compartment: ModelProductCompartmentSite): Boolean = {
    reactant_compartment match {
      case ModelProducedCompartmentSite(produced) =>
        Observed.contains(produced)

      case ModelChangedCompartmentSite(consumed, produced) =>
        Observed.contains(produced) && !consumed.diff(Observed).isEmpty

      case _: ModelStaticCompartmentSite =>
        false
    }
  }

  override def toString = "@" + Static.map(_.Name).mkString("{",",","}")
}

