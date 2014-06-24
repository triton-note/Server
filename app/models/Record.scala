package models

import play.api.libs.json._

case class Record(
  comment: String,
  date: java.util.Date,
  location: String,
  geoinfo: GeoInfo,
  photo: Option[String],
  catches: Seq[Record.Catches]) {
}
object Record {
  case class ValueUnit(value: Double, unit: String) {
    def tupled = (value, unit)
  }
  object ValueUnit {
    implicit val valueunitFormat = Json.format[ValueUnit]
    def tupled(t: (Double, String)) = ValueUnit(t._1, t._2)
  }
  case class Catches(
    name: String,
    count: Int,
    weight: Option[ValueUnit],
    length: Option[ValueUnit]) {
  }
  object Catches {
    implicit val catchesFormat = Json.format[Catches]
  }
  implicit val recordFormat = Json.format[Record]
}