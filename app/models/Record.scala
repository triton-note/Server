package models

import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper

case class Record(
  comment: String,
  date: java.util.Date,
  location: String,
  geoinfo: GeoInfo,
  catches: Seq[Record.Catches]) {
}
object Record {
  case class Catches(
    name: String,
    count: Int,
    weight: Option[Double],
    weightUnit: String,
    length: Option[Double],
    lengthUnit: String) {
  }
  object Catches {
    implicit val catchesFormat = Json.format[Catches]
  }
  implicit val recordFormat = Json.format[Record]
}