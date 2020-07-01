/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model

sealed trait EdgeSite

object EdgeSite {
  def apply(edge: Edge) = {
    edge match {
      case UnboundEdge       => TrueLocalSiteEdge(UnboundEdge)
      case edge: BoundEdge   => BoundSiteEdge(edge)
      case edge: PartialEdge => TrueLocalSiteEdge(edge)
    }
  }
}

case class AnySiteEdge() extends EdgeSite {
  override def toString = "-?"
}

case class BoundSiteEdge(edge: BoundEdge) extends EdgeSite {
  override def toString = "-" + edge
}

case class TrueLocalSiteEdge(edges: Seq[LocalEdge]) extends EdgeSite {

  def this(edge: LocalEdge) = this(Seq(edge))

  override def toString = {
    edges match {
      case Seq(UnboundEdge) => ""
      case Seq(edge)          => "-" + edge
      case list               => "-" + list.mkString("{",",","}")
    }
  }
}

object TrueLocalSiteEdge {
  def apply(edge: LocalEdge) = new TrueLocalSiteEdge(edge)
}

case class NegatedLocalSiteEdge(edges: Seq[LocalEdge]) extends EdgeSite {
  override def toString = {
    edges match {
      case Seq(edge) => "-!" + edge
      case list      => "-!" + list.mkString("{",",","}")
    }
  }
}
