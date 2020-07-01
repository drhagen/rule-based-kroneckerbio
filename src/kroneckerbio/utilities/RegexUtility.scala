package kroneckerbio.utilities

import scala.util.matching.Regex

/**
 * Created with IntelliJ IDEA.
 * User: David Hagen
 * Date: 5/17/12
 * Time: 3:41 PM
 * To change this template use File | Settings | File Templates.
 */

object RegexUtility {
  class RichRegex(underlying: Regex) {
    def contains(s: String) = underlying.pattern.matcher(s).matches
  }

  implicit def regexToRichRegex(r: Regex) = new RichRegex(r)
}
