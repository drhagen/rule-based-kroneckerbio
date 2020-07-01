/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.utilities

import collection.mutable.ArrayBuffer

object ErrorBuffer {
  abstract class ErrorElement() {
    def invalid: Boolean
    def check(crash:Boolean, padding: String): Boolean
  }

  case class NoError() extends ErrorElement {
    val invalid = false

    def check(crash: Boolean = true, padding: String = "") = false
  }

  case class ConcreteError(message: String, invalid: Boolean = true) extends ErrorElement {

    def check(crash: Boolean = true, padding: String = ""): Boolean = {
      if (invalid) {
        println(padding + message)

        // Throw an error if requested
        if (crash) {
          throw new Error()
        }
      }

      return invalid
    }
  }

  case class ErrorBuffer(message: Option[String] = None) extends ErrorElement {
    val buffer = ArrayBuffer[ErrorElement]()


    def invalid = !buffer.isEmpty

    def ++(newBuffer: ErrorElement): ErrorBuffer = {
      if (newBuffer.invalid) {
        buffer += newBuffer
      }
      this
    }

    def ++(listOfBuffers: Iterable[ErrorElement]): ErrorBuffer = {
      listOfBuffers.foreach(
        this ++ _
      )
      this
    }

    // Print all the errors at once, if any
    // Throw an error if requested
    def check(crash: Boolean = true, padding: String = ""): Boolean = {
      if (invalid) {
        // Print header
        message match {
          case Some(message) => println(padding + message)
          case None => Unit
        }

        // Print all other messages
        buffer.foreach(
          _.check(false, padding + "  ")
        )

        // Throw an error if requested
        if (crash) {
          throw new Error()
        }
      }

      return invalid
    }
  }

}
