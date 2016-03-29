package json

sealed trait JsonValue
case class JsonObject(elem: Map[String, JsonValue]) extends JsonValue
case class JsonArray(elems: List[JsonValue]) extends JsonValue
case class JsonString(value: String) extends JsonValue
case class JsonNumber(value: BigDecimal) extends JsonValue


object JsonWriter {
  def write(json: JsonValue): String = json match {
    case JsonObject(elems) =>
      val entries = for {
        (key, value) <- elems
      } yield s""""$key: ${write(value)}""""
      "{" + entries.mkString(", ") + "}"
    case JsonArray(elems) =>
      val entries = elems.map(write)
      "[" + entries.mkString(", ") + "]"
    case JsonString(value) => s""""$value""""
    case JsonNumber(value) => value.toString
  }
}

trait JsonSerializable {
  def toJson: JsonValue
}

object JsonSerializer {
  def write(serializable: JsonSerializable) =
    JsonWriter.write(serializable.toJson)
}
