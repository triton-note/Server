package models

import play.api.libs.json._

case class Report(
  id: Option[String],
  comment: String,
  dateAt: java.util.Date,
  location: Report.Location,
  photo: Option[Report.Photo],
  fishes: Seq[Report.Fishes]) {
}
object Report {
  case class Location(name: String, geoinfo: GeoInfo)
  object Location {
    implicit val locationFormat = Json.format[Location]
  }
  case class ValueUnit(value: Double, unit: String) {
    def tupled = (value, unit)
  }
  object ValueUnit {
    implicit val valueunitFormat = Json.format[ValueUnit]
    def tupled(t: (Double, String)) = ValueUnit(t._1, t._2)
  }
  case class Photo(
    original: String,
    reduced: String,
    thumbnail: String)
  object Photo {
    implicit val photoFormat = Json.format[Photo]
  }
  case class Fishes(
    name: String,
    count: Int,
    weight: Option[ValueUnit] = None,
    length: Option[ValueUnit] = None) {
  }
  object Fishes {
    implicit val catchesFormat = Json.format[Fishes]
  }
  implicit val reportFormat = Json.format[Report]
}