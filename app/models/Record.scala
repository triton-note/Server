package models

import play.api.libs.json._

case class Record(
  comment: String,
  dateAt: java.util.Date,
  location: Record.Location,
  photo: Option[String],
  fishes: Seq[Record.Fishes]) {
}
object Record {
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
  case class Fishes(
    name: String,
    count: Int,
    weight: Option[ValueUnit] = None,
    length: Option[ValueUnit] = None) {
  }
  object Fishes {
    implicit val catchesFormat = Json.format[Fishes]
  }
  implicit val recordFormat = Json.format[Record]
}