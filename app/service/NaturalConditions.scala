package service

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scalaz.Scalaz._

import play.api.Logger
import play.api.Play.current
import play.api.libs.ws.{WS, WSResponse}

import org.fathens.astronomy.Moon
import org.fathens.math._
import org.fathens.play.util.Exception.allCatch

import models.GeoInfo
import models.Report.Condition
import models.Report.Condition.{Tide, Weather}

object NaturalConditions {
  lazy val WEATHER_API_KEY = System.getProperty("OPENWEATHERMAP_APPID")

  def weatherIcon(id: String) = f"http://openweathermap.org/img/w/${id}.png"

  object parse {
    def JSON(res: WSResponse) = res.status match {
      case 200 => allCatch opt res.json
      case _ =>
        Logger error f"Status(${res.status}) ${res.body}"
        None
    }
  }

  def tideState(origin: Degrees, moon: Degrees): Tide.Value = {
    def angle: Degrees = {
      val diff = moon - origin + Degrees(15)
      diff.normalize % Pi
    }
    Logger debug f"TideMoon origin(${origin}) -> moon(${moon}): ${angle}"
    angle.toDouble match {
      case d if d < 30             => Tide.High
      case d if 30 <= d && d <= 90 => Tide.Flood
      case d if 90 < d && d < 120  => Tide.Low
      case d if 120 <= d           => Tide.Ebb
    }
  }
  def weather(date: java.util.Date, geoinfo: GeoInfo): Future[Option[Weather]] = {
    val ws = WS.client url "http://api.openweathermap.org/data/2.5/history/city"
    ws.withQueryString(
      "lat" -> f"${geoinfo.latitude.toDouble}%0.5f",
      "lon" -> f"${geoinfo.longitude.toDouble}%0.5f",
      "type" -> "hour",
      "start" -> date.getTime.toString,
      "cnt" -> "1",
      "APPID" -> WEATHER_API_KEY
    ).get().map(parse.JSON).map {
        _ flatMap { json =>
          ((json \ "cnt").as[Int] <= 0) option {
            val info = (json \ "list")(0)
            val name = (info \ "weather" \ "main").as[String]
            val temp = (info \ "main" \ "temp").as[Double] - 273.15
            val icon = (info \ "weather" \ "icon").as[String]
            Weather(name, temp, weatherIcon(icon))
          }
        }
      }
  }
  def at(date: java.util.Date, geoinfo: GeoInfo): Future[Condition] = {
    weather(date, geoinfo).map(_ getOrElse Weather("Fine", 20.0, weatherIcon("01d"))) map { weather =>
      val moon: Moon = new Moon(date)
      val tide: Tide.Value = tideState(geoinfo.longitude, moon.earth_longitude)
      Condition(moon.age.round.toInt, tide, weather)
    }
  }
}
