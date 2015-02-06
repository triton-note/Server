package models

import java.util.Date

import play.api.libs.json._

case class Report(
  id: Option[String],
  comment: String,
  dateAt: Date,
  location: Report.Location,
  condition: Report.Condition,
  photo: Option[Report.Photo],
  fishes: Seq[Report.Fishes]) {
}
object Report {
  case class Location(name: String, geoinfo: GeoInfo)
  object Location {
    implicit val locationFormat = Json.format[Location]
  }
  case class Condition(moon: Int, tide: Condition.Tide.Value, weather: Option[Condition.Weather]) {
    lazy val asJson = Json toJson this
  }
  object Condition {
    object Tide extends Enumeration {
      val Flood = Value("Flood")
      val High = Value("High")
      val Ebb = Value("Ebb")
      val Low = Value("Low")
      implicit val tideFormat = Format(
        (__).read[String].map(Tide.withName),
        Writes { (t: Tide.Value) => JsString(t.toString) })
    }
    case class Temperature(value: Double, unit: MeasureUnit.Temperature.Value)
    object Temperature {
      implicit val json = Json.format[Temperature]
    }
    case class Weather(name: String, temperature: Temperature, iconUrl: String)
    object Weather {
      implicit val weatherFormat = Json.format[Weather]
    }
    implicit val json = Json.format[Condition]
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
    mainview: String,
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