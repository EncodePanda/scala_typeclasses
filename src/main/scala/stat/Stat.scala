package stat

trait Number[A] {
  def plus(n1: A, n2: A): A
  def divide(n1: A, n2: A): A
  def divide(n1: A, n2: Int): A
}

object Number {

  implicit object BigDecimalNumber extends Number[BigDecimal] {
    def plus(n1: BigDecimal, n2: BigDecimal): BigDecimal = n1 + n2
    def divide(n1: BigDecimal, n2: BigDecimal): BigDecimal = n1 / n2
    def divide(n1: BigDecimal, n2: Int): BigDecimal = n1 / n2
  }

  implicit object IntNumber extends Number[Int] {
    def plus(n1: Int, n2: Int): Int = n1 + n2
    def divide(n1: Int, n2: Int): Int = n1 / n2
  }

  implicit object DoubleNumber extends Number[Double] {
    def plus(n1: Double, n2: Double): Double = n1 + n2
    def divide(n1: Double, n2: Double): Double = n1 / n2
    def divide(n1: Double, n2: Int): Double = n1 / n2
  }

  implicit class NumberOps[A](a: A) {
    def +(other: A)(implicit number: Number[A]) = number.plus(a, other)
    def /(other: A)(implicit number: Number[A]) = number.divide(a, other)
    def /(other: Int)(implicit number: Number[A]) = number.divide(a, other)
  }
}

object Stat {
  import Number._
  def mean[A](xs: Seq[A])(implicit number: Number[A]): A =
    xs.reduce(_ + _) / xs.size
}
