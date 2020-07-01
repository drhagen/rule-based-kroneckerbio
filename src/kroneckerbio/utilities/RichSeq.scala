/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.utilities

class RichSeq[A](underlying: Seq[A]) {
  def allExists[B](seq: Seq[B], test: (A, B) => Boolean): Boolean =
    seq.forall(source => underlying.exists(target => test(target, source)))

  def isUnique = underlying.size == underlying.distinct.size

  def duplicates = underlying.distinct.diff(underlying)

  def FindOrDie(p: A => Boolean) = underlying.find(p).get

  def CombinationsFromEach[B](implicit ev: A <:< Seq[B]): Seq[Seq[B]] = {
    underlying.foldLeft(Seq(Seq.empty[B])){
      (current_lists, next_list) =>
        for {
          list <- current_lists
          item <- next_list
        } yield {
          list :+ item
        }
    }
  }
}

object RichSeq {
  implicit def seqToRichSeq[A](r: Seq[A]) = new RichSeq(r)
}
