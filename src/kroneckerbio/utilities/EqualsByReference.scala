/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.utilities

trait EqualsByReference {
  override def equals(obj: Any): Boolean = super.equals(obj)
  override def hashCode = super.hashCode
}
