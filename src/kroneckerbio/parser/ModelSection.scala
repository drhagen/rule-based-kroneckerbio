package kroneckerbio.parser

import kroneckerbio.model._
import kroneckerbio.utilities.NegatableSeq._

/**
 * Created with IntelliJ IDEA.
 * User: David Hagen
 * Date: 5/14/12
 * Time: 2:25 PM
 * To change this template use File | Settings | File Templates.
 */

sealed abstract class ModelSection

case class CompartmentSection(lines: List[Compartment], name: Option[String]) extends ModelSection

case class AgentSection(lines: List[Agent]) extends ModelSection

case class SeedSection(lines: List[ParserSeed], compartment: Option[CompartmentName]) extends ModelSection

case class ObservableSection(lines: List[Observable], compartment: Option[CompartmentSite]) extends ModelSection

case class ParameterSection(lines: List[Parameter]) extends ModelSection

case class RuleSection(lines: List[Rule]) extends ModelSection
