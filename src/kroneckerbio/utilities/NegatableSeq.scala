package kroneckerbio.utilities

import kroneckerbio.implicitOption
import kroneckerbio.utilities.RichSeq._

object NegatableSeq {
  case class NegatableSeq[+A](list: Seq[A], literal: Boolean) {
    def isEmpty = list.isEmpty

    override def equals(that: Any) = that match {
      // If both are Nil, then they are equal no matter what
      case NegatableSeq(Seq(), _) => list.isEmpty
      case NegatableSeq(that_list,that_literal) => that_list == list && that_literal == literal
      case _ => false
    }

    def contains[B >: A](elem: B) = list.find(_ == elem) match {
      case Some(_) => literal
      case None => !literal
    }

    def :+[B >: A](elem: B) = NegatableSeq[B](list :+ elem, literal)

    // Return what the set represents
    def resolve[B >: A](completeList: Traversable[B]) =
      if (literal) {
        completeList.toSeq intersect list
      }
      else {
        completeList.toSeq diff list
      }

    def mkString =
      if (list.isEmpty) {
        ""
      }
      else {
        (if (!literal) {
          "!"
        } else {
          ""
        }) +
          (list match {
            case Seq(item) => "" + item
            case items => items.mkString("{", ",", "}")
          })
      }
  }
  object NegatableSeq {
    def apply[A](
      list: Option[Traversable[A]] = None,
      literal: Option[Boolean] = None
    ): NegatableSeq[A] = NegatableSeq(
      list.getOrElse(Nil).toSeq,
      literal.getOrElse(true)
    )
  }

  implicit def listToNegatableSeq[A](list: Traversable[A]) = NegatableSeq[A](list)
  //implicit def anyToNegatableSeq[A](obj: A) = NegatableSeq[A](Seq(obj))
}
