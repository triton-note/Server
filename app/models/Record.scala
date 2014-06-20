package models

import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper

case class Record(
  comment: String,
  location: String,
  geoinfo: Option[GeoInfo],
  catches: Seq[Record.Catches]) {
}
object Record {
  case class Catches(
    name: String,
    count: Int,
    weight: Option[Double],
    length: Option[Double]) {

    def toJson = Json.obj(
      "name" -> name,
      "count" -> count,
      "weight" -> weight,
      "length" -> length
    )
  }
  object Catches {
    implicit val catchesFormat = Json.format[Catches]
  }
  implicit val recordFormat = Json.format[Record]
}