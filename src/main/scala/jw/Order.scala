package jw

import json._
import stat.Stat

trait Evaluatable[T] {
  def evaluate: T
}

sealed trait Product extends Evaluatable[BigDecimal] with JsonSerializable {
  def evaluate: BigDecimal
}

case class BasicProduct(id: Int, price: BigDecimal) extends Product {
  def evaluate: BigDecimal = price
  def toJson: JsonValue = JsonObject(Map(
    "id" -> JsonNumber(BigDecimal(id)),
    "price" -> JsonNumber(price)
  ))
}

case class DiscountedProduct(product: Product, discount: Double) extends Product {
  def evaluate: BigDecimal = product.evaluate * (1 - discount)
  def toJson: JsonValue = JsonObject(Map(
    "product" -> product.toJson,
    "discount" -> JsonNumber(discount)
  ))
}

case object OutOfStock extends Product {
  def evaluate: BigDecimal = BigDecimal("0.0")
  def toJson: JsonValue = JsonString("out of stock")
}

sealed trait Order extends Evaluatable[BigDecimal] with JsonSerializable {
  def evaluate: BigDecimal
}

case class GeneralOrder(products: List[Product]) extends Order {
  def evaluate: BigDecimal = products.foldLeft(BigDecimal("0.0")) {
    case (acc, p) => acc + p.evaluate
  }

  def toJson: JsonValue = JsonArray(products.map(_.toJson))
}

case object CancelledOrder extends Order {
  def evaluate: BigDecimal = BigDecimal("0.0")
  def toJson: JsonValue = JsonString("cancelled")
}

case class ComplexOrder(orders: List[Order]) extends Order {
  def evaluate: BigDecimal = orders.foldLeft(BigDecimal("0.0")) {
    case (acc, o) => acc + o.evaluate
  }

  override def toJson: JsonValue = JsonArray(orders.map(_.toJson))
}

object Order {
  def average(orders: Seq[Order]): BigDecimal =
    Stat.mean(orders.map(_.evaluate))
}
