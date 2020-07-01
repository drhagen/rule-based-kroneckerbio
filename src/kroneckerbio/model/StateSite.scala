/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.model

import kroneckerbio.parser.RuleBasedModelParser
import kroneckerbio.utilities.RichSeq._
import kroneckerbio.utilities.RegexUtility._

sealed trait StateSite {
  def AllStateNames: Seq[StateName]
}

case class AnyStateSite() extends StateSite {
  def AllStateNames = Nil

  override def toString = "~?"
}

case class TrueStateSite(states: Seq[StateName]) extends StateSite {
  require(states.isUnique, "Sites have duplicate state names")

  def this(state: StateName) = this(Seq(state))

  def AllStateNames = states

  override def toString = {
    states match {
      case Seq(StateName("0")) => ""
      case Seq(state)          => "~" + state
      case list                => "~" + list.mkString("{",",","}")
    }
  }
}

object TrueStateSite {
  def apply(state: StateName) = new TrueStateSite(state)
}

case class NegatedStateSite(states: Seq[StateName]) extends StateSite {
  require(states.isUnique, "Sites have duplicate state names")

  def AllStateNames = states

  override def toString = {
    states match {
      case Seq(state) => "~!" + state
      case list       => "~!" + list.mkString("{",",","}")
    }
  }
}
