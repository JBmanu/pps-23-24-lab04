package tasks.adts

package u04lab

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    type Real = Double
    type Imaginary = Double

    type Complex = ComplexImpl
    case class ComplexImpl(real: Real, imaginary: Imaginary)
    def complex(re: Real, im: Imaginary): Complex = ComplexImpl(re, im)

    extension (complex: Complex)
      def re(): Real = complex.real
      def im(): Imaginary = complex.imaginary
      def sum(other: Complex): Complex =
        ComplexImpl(complex.re() + other.re(), complex.im() + other.im())
      def subtract(other: Complex): Complex =
        ComplexImpl(complex.re() - other.re(), complex.im() - other.im())
      def asString(): String =
        complex match
          case ComplexImpl(_, 0) => s"${complex.re()}"
          case ComplexImpl(0, _) => s"${complex.im()}i"
          case _ =>
            val sign = if complex.im() >= 0 then "+" else "-"
            s"${complex.re()} $sign ${Math.abs(complex.im())}i"

