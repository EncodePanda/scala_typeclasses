package stat

object Stat {
  // def median(xs: Seq[BigDecimal]): BigDecimal = xs(xs.size / 2)
  def mean(xs: Seq[BigDecimal]): BigDecimal = 
    xs.reduce(_ + _) / xs.size
  
}
