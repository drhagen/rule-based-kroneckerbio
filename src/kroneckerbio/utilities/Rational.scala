/**
 * (c) 2012 David R Hagen & Bruce Tidor
 * This work is released under the MIT license.
 */
package kroneckerbio.utilities

object Rational {
  case class Rational(numer: Int, denom: Int = 1) {
    require(denom != 0)

    val (n, d) = {
      val g = gcd(numer.abs, denom.abs)

      (numer / g, denom / g)
    }

    def + (that: Rational): Rational =
      new Rational(
      n * that.d + that.n * d,
      d * that.d
    )

    def + (i: Int): Rational =
      new Rational(n + i * d, d)

    def - (that: Rational): Rational =
      new Rational(n * that.d - that.n * d, d * that.d)

    def - (i: Int): Rational =
      new Rational(n - i * d, d)

    def * (that: Rational): Rational =
      new Rational(n * that.n, d * that.d)

    def * (i: Int): Rational =
      new Rational(n * i, d)

    def / (that: Rational): Rational =
      new Rational(n * that.d, d * that.n)

    def / (i: Int): Rational =
      new Rational(n, d * i)

    override def toString = n +"/"+ d

    private def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)
  }

  implicit object RationalNumeric extends Numeric[Rational] {
    def fromInt(x: Int) = Rational(x)
    def toInt(x: Rational) = x.n / x.d
    def toLong(x: Rational) = x.n.toLong / x.d.toLong
    def toFloat(x: Rational) = x.n.toFloat / x.d.toFloat
    def toDouble(x: Rational) = x.n.toDouble / x.d.toDouble
    def times(x1: Rational, x2: Rational) = x1 * x2
    def plus(x1: Rational, x2: Rational) = x1 + x2
    def minus(x1: Rational, x2: Rational) = x1 - x2
    def compare(x1: Rational, x2: Rational) = toDouble(x1).compare(toDouble(x2))
    def negate(x1: Rational) = Rational(-x1.n, x1.d)
  }

  implicit def int2Rational(int: Int): Rational = Rational(int)
  implicit def rational2Float(rat: Rational): Double = rat.n.toFloat / rat.d
}
