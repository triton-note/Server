package models

import java.util.Date

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api.libs.json._

import org.fathens.play.util.Exception.allCatch

import service.NaturalConditions

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
  case class Condition(moon: Int, tide: Condition.Tide.Value, weather: Condition.Weather) {
    lazy val asJson = allCatch opt Json.toJson(this)
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
    case class Weather(name: String, temperature: Double)
    object Weather {
      implicit val weatherFormat = Json.format[Weather]
    }
    /**
     * Create by datetime and geolocation
     */
    def at(datetime: Date, geoinfo: GeoInfo): Future[Condition] = {
      val nc = new NaturalConditions(datetime, geoinfo)
      nc.weather.map { weather =>
        val moon = nc.moon.age.round.toInt
        val tide = Tide withName nc.tide.toString
        Condition(moon, tide, weather)
      }
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