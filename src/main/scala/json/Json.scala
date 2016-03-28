package json

sealed trait JsonValue
case class JsonObject(elem: Map[String, JsonValue]) extends JsonValue
case class JsonArray(elems: List[JsonValue]) extends JsonValue
case class JsonString(value: String) extends JsonValue
case class JsonNumber(value: BigDecimal) extends JsonValue
case object JsonNull extends JsonValue


object JsonWriter {
  def write(json: JsonValue): String = json.toString
}

trait JsonSerializable {
  def toJson: JsonValue
}

object JsonSerializer {
  def write(serializable: JsonSerializable) =
    JsonWriter.write(serializable.toJson)
}
