/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.utilities

import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer

case class WatchableBuffer[A](array: Buffer[A], delegates: Buffer[() => Unit]) extends Buffer[A] {
  def this(array: Buffer[A], delegates: () => Unit) = this(array, ArrayBuffer(delegates))

  // Array functions
  def +=(elem: A) = {trigger(); array += elem; this}
  def +=:(elem: A) = {trigger(); elem +=: array; this}
  def apply(m: Int) = array.apply(m)
  def clear() {trigger(); array.clear()}
  def insertAll(n: Int, elems: Traversable[A]) {trigger(); array.insertAll(n,elems)}
  def iterator = array.iterator
  def length = array.length
  def remove(n: Int) = {trigger(); array.remove(n)}
  def update(n: Int, newelem: A) {trigger(); array.update(n,newelem)}

  // Event functions
  def trigger() {delegates.foreach(_.apply())}
  def AddDelegate(delegate: () => Unit) = delegates += delegate
  def RemoveDelegate(delegate: () => Unit) = delegates -= delegate
}
