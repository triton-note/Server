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
  trait ValueUnit[U] {
    val value: Double
    val unit: U
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
    case class TemperatureValue(value: Double, unit: MeasureUnit.Temperature.Value) extends ValueUnit[MeasureUnit.Temperature.Value]
    object TemperatureValue {
      implicit val json = Json.format[TemperatureValue]
    }
    case class Weather(name: String, temperature: TemperatureValue, iconUrl: String)
    object Weather {
      implicit val weatherFormat = Json.format[Weather]
    }
    implicit val json = Json.format[Condition]
  }
  case class LengthValue(value: Double, unit: MeasureUnit.Length.Value) extends ValueUnit[MeasureUnit.Length.Value]
  object LengthValue {
    implicit val json = Json.format[LengthValue]
  }
  case class WeightValue(value: Double, unit: MeasureUnit.Weight.Value) extends ValueUnit[MeasureUnit.Weight.Value]
  object WeightValue {
    implicit val json = Json.format[WeightValue]
  }
  object SizeValue {
    implicit val json = Json.format[SizeValue]
  }
  case class SizeValue(weight: Option[WeightValue], length: Option[LengthValue]) {
    override def toString = List(weight, length).flatten.map { vu =>
      f"${vu.value}%10.1f ${vu.unit}"
    } match {
      case Nil  => ""
      case list => list.mkString(", ")
    }
    lazy val asJson = Json toJson this
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
    weight: Option[WeightValue] = None,
    length: Option[LengthValue] = None) {
  }
  object Fishes {
    implicit val catchesFormat = Json.format[Fishes]
  }
  implicit val reportFormat = Json.format[Report]
}
