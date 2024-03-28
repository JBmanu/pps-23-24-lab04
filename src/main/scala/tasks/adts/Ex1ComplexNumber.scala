package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumber:

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
    opaque type Real = Double
    opaque type Imaginary = Double
    opaque type Complex = ComplexImpl

    case class ComplexImpl(real: Real, imaginary: Imaginary)

    def complex(re: Real, im: Imaginary): Complex = ComplexImpl(re, im)

    extension (complex: Complex)
      def re(): Real = complex.real
      def im(): Imaginary = complex.imaginary
      def sum(other: Complex): Complex =
        ComplexImpl(complex.re() + other.re(), complex.im() + other.im())
      def subtract(other: Complex): Complex =
        ComplexImpl(complex.re() - other.re(), complex.im() - other.im())

      private def spaceSign(n: Double): String =
        val sign = if complex.im () >= 0 then "+" else "-"
        s"$sign ${Math.abs(n)}"

      def asString(): String =
        complex match
          case ComplexImpl(re, 0) => s"$re"
          case ComplexImpl(0, im) => s"${im}i"
          case _                  =>
            s"${complex.re()} ${spaceSign(complex.im())}i"

