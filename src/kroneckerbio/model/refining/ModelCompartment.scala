/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model.refining

import kroneckerbio.model._
import kroneckerbio.utilities.EqualsByReference
import kroneckerbio.utilities.RichSeq._

case class ModelCompartment(
  Name: CompartmentName,
  Dimension: Int,
  Size: Double,
  var Container: Option[ModelCompartment],
  var Members: Seq[ModelCompartment]
) extends EqualsByReference {

  def AdjacentCompartments = Members ++ Container

  override def toString = {
    Name + " " +
    Dimension + " " +
    Size +
    (Container match {
      case Some(container) => " " + container.Name
      case None            => ""
    })
  }
}

object ModelCompartment {
  def apply(original: Compartment) = {
    new ModelCompartment(
      original.Name,
      original.Dimension,
      original.Size,
      None,
      Nil
    )
  }

  def BuildCompartments(original_compartments: Seq[Compartment]): Seq[ModelCompartment] = {
    // Convert all compartments
    val initial_compartments = original_compartments.map(ModelCompartment(_))

    // Map the old compartments onto the new compartments
    val compartment_map = original_compartments.zip(initial_compartments).toMap

    // Group all compartments by their container
    val container_map = original_compartments.groupBy(_.Container)

    for {
      entry <- container_map
      if (entry._1 != None)
    } {
      val container = original_compartments.FindOrDie(_.Name == entry._1.get)
      val members = entry._2

      // Add the members to the container
      compartment_map(container).Members = members.map(compartment_map(_))

      // Add the container to the members
      members.map(compartment_map(_).Container = Some(compartment_map(container)))
    }

    // Return
    initial_compartments
  }
}
