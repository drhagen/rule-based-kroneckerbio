/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.utilities

object RichAny {
  class PipedObject[T] private[RichAny] (value:T) {
    def pipe[R] (f : T => R) = f(this.value)
  }

  implicit def toPiped[T] (value:T) = new PipedObject[T](value)
}
