package stat

trait Number[A] {
  def value: A
  def +(other: Number[A]): Number[A]
  def /(other: Number[A]): Number[A]
  def /(other: Int): Number[A]
}

case class BigDecimalNumber(value: BigDecimal) extends Number[BigDecimal] {
  def +(other: Number[BigDecimal]) = BigDecimalNumber(value + other.value)
  def /(other: Number[BigDecimal]) = BigDecimalNumber(value / other.value)
  def /(other: Int) = BigDecimalNumber(value / other)
}

case class IntNumber(value: Int) extends Number[Int] {
  def +(other: Number[Int]) = IntNumber(value + other.value)
  def /(other: Number[Int]) = IntNumber(value / other.value)
  def /(other: Int) = IntNumber(value / other)
}

case class DoubleNumber(value: Double) extends Number[Double] {
  def +(other: Number[Double]) = DoubleNumber(value + other.value)
  def /(other: Number[Double]) = DoubleNumber(value / other.value)
  def /(other: Int) = DoubleNumber(value / other)
}


object Stat {
  def mean[A](xs: Seq[Number[A]]): Number[A] =
    xs.reduce(_ + _) / xs.size
}
