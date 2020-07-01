package kroneckerbio.utilities

/**
 * Created with IntelliJ IDEA.
 * User: David Hagen
 * Date: 5/25/12
 * Time: 12:40 PM
 * To change this template use File | Settings | File Templates.
 */

object NegatableSet {

  case class NegatableSet[A](set: Set[A], literal: Boolean = true) {
    def equals(that: NegatableSet[A]) = (
      if (that.set.isEmpty) (
        set.isEmpty
      )
      else (
        that.set == set && that.literal == literal
      )
    )

    def +(elem: A) = new NegatableSet(set + elem, literal)

    def contains(elem: A) = set.find(_ == elem) match {
      case Some(_) => literal
      case None    => !literal
    }

    // Return what the set represents
    def resolve(completeSet: Iterable[A]) =
      if (literal) {
        completeSet.toSet intersect set
      }
      else {
        completeSet.toSet diff set
      }

    def mkString =
      if (set.isEmpty) {
        ""
      }
      else {
        (if (!literal) {
          "!"
        } else {
          ""
        }) + (
        if (set.size == 1) (
          "" + set.head // WTF Scala?
        )
        else (
          set.mkString("{", ",", "}")
        ))
      }
  }

}
