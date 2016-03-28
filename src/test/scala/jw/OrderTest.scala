package jw

import org.scalatest.{FunSuite, Matchers}

class OrderTest extends FunSuite with Matchers {

  test("should evaluate order") {
    val o1 = GeneralOrder(BasicProduct(10, BigDecimal("10.2")) :: Nil)
    val o2 = GeneralOrder(DiscountedProduct(product = BasicProduct(11, BigDecimal("1")), discount = 0.2) :: Nil)
    val o3 = CancelledOrder
    val order = ComplexOrder(o1 :: o2 :: o3 :: Nil)

    order.evaluate should equal(BigDecimal("11.0"))
  }

  test("should calculate average") {
    val o1 = GeneralOrder(DiscountedProduct(product = BasicProduct(11, BigDecimal("1")), discount = 0.2) :: Nil)
    val o2 = GeneralOrder(BasicProduct(10, BigDecimal("4")) :: Nil)
    val o3 = GeneralOrder(BasicProduct(10, BigDecimal("10.2")) :: Nil)

    Order.average(o1 :: o2 :: o3 :: Nil) should equal(BigDecimal("5.0"))
  }

}
