/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model

case class Compartment(Name: CompartmentName, Dimension: Int, Size: Double, Container: Option[CompartmentName] = None) {
  require(Dimension >= 0, "Compartment dimension " + Dimension + " is invalid")
  require(Size >= 0, "Compartment size " + Size + " is invalid")

  override def toString = Container match {
    case Some(container) => Name + " "  + Dimension + " " + Size + " " + container
    case None => Name + " "  + Dimension + " " + Size
  }
}
