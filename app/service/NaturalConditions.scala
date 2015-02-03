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
  lazy val WEATHER_API_URL = System.getenv("OPENWEATHERMAP_URL")
  lazy val WEATHER_API_KEY = System.getenv("OPENWEATHERMAP_APPID")

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
    (WS.client url WEATHER_API_URL).withQueryString(
      "lat" -> f"${geoinfo.latitude.toDouble}%3.8f",
      "lon" -> f"${geoinfo.longitude.toDouble}%3.8f",
      "type" -> "hour",
      "start" -> f"${date.getTime / 1000}",
      "cnt" -> "1",
      "APPID" -> WEATHER_API_KEY
    ).get().map(parse.JSON).map {
        _ flatMap { json =>
          Logger debug f"Weather Result of (${geoinfo} at ${date}): ${json}"
          (json \ "cnt").as[Int] > 0 option {
            val info = (json \ "list")(0)
            val wth = (info \ "weather")(0)
            val name = (wth \ "main").as[String]
            val icon = (wth \ "icon").as[String]
            val temp = (info \ "main" \ "temp").as[Double] - 273.15
            Weather(name, temp, weatherIcon(icon))
          }
        }
      }
  }
  def at(date: java.util.Date, geoinfo: GeoInfo): Future[Condition] = {
    weather(date, geoinfo).map(_ getOrElse Weather("Clear", 20.0, weatherIcon("01d"))) map { weather =>
      val moon: Moon = new Moon(date)
      val tide: Tide.Value = tideState(geoinfo.longitude, moon.earth_longitude)
      Condition(moon.age.round.toInt, tide, weather)
    }
  }
}
