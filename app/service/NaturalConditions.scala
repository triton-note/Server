package service

import java.util.Date

import scala.concurrent.Future
import scala.concurrent.duration._

import scalaz.Scalaz._

import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.libs.ws.WS

import org.fathens.astronomy.Moon
import org.fathens.math._
import org.fathens.play.util.Exception.allCatch

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import models.GeoInfo
import models.Report.Condition
import models.Report.Condition.{ Tide, Weather }

object NaturalConditions {
  object OpenWeatherMap {
    lazy val URL = System.getenv("OPENWEATHERMAP_URL")
    lazy val API_KEY = System.getenv("OPENWEATHERMAP_APPID")
    lazy val ICON = System.getenv("OPENWEATHERMAP_ICON_URL")
    def icon(id: String) = f"${ICON}/${id}.png"

    /**
     * OpenWeatherMap.com API
     */
    private def get(subpath: String, geoinfo: GeoInfo)(parameters: (String, String)*)(reciever: JsValue => Option[Weather]): Future[Either[Int, Option[Weather]]] = {
      val params = "APPID" -> API_KEY ::
        "lat" -> f"${geoinfo.latitude.toDouble}%3.8f" ::
        "lon" -> f"${geoinfo.longitude.toDouble}%3.8f" ::
        parameters.toList
      val paramString = params.map { case (a, b) => f"${a}=${b}" }.mkString(", ")
      val path = f"${URL}/${subpath}"
      Logger info f"GET: ${path}: ${paramString}"
      (WS.client url path).withQueryString(params: _*).get().map { res =>
        res.status match {
          case 200 => Right((allCatch opt reciever(res.json)).flatten)
          case error =>
            Logger error f"Request for ${path}(${paramString}) => Status(${res.status}) ${res.body}"
            Left(error)
        }
      }
    }
    def getPast(date: Date, geoinfo: GeoInfo) = get("history/city", geoinfo)(
      "type" -> "hour",
      "start" -> f"${date.getTime / 1000}",
      "cnt" -> "1"
    ) { json =>
        Logger debug f"Weather Result of (${geoinfo} at ${date}): ${json}"
        (json \ "cnt").as[Int] > 0 option {
          val info = (json \ "list")(0)
          val wth = (info \ "weather")(0)
          val name = (wth \ "main").as[String]
          val id = (wth \ "icon").as[String]
          val temp = (info \ "main" \ "temp").as[Double] - 273.15
          Weather(name, temp, icon(id))
        }
      }
    def getCurrent(geoinfo: GeoInfo) = get("weather", geoinfo)() { json =>
      Logger debug f"Weather Result of (${geoinfo}): ${json}"
      Option {
        val wth = (json \ "weather")(0)
        val name = (wth \ "main").as[String]
        val id = (wth \ "icon").as[String]
        val temp = (json \ "main" \ "temp").as[Double] - 273.15
        Weather(name, temp, icon(id))
      }
    }
    def apply(delay: FiniteDuration, countMax: Int)(date: Date, geoinfo: GeoInfo) = {
      def retry(task: => Future[Either[Int, Option[Weather]]])(count: Int): Future[Option[Weather]] = task.flatMap {
        case Right(w) => Future(w)
        case Left(_) => if (count < 1) Future(None) else {
          val sch = Akka.system.actorOf(Props(new Actor {
            def receive = {
              case "go" =>
                val sed = sender
                Akka.system.scheduler.scheduleOnce(delay) {
                  retry(task)(count - 1).map(sed ! _)
                }
            }
          }))
          Logger warn f"Retry left: ${count} after ${delay}"
          implicit val timeout = Timeout(delay * count * 2)
          (sch ? "go").mapTo[Option[Weather]]
        }
      }
      if (new Date().getTime - date.getTime < 3 * 60 * 60 * 1000) {
        retry(getCurrent(geoinfo))(countMax)
      } else retry(getPast(date, geoinfo))(countMax)
    }
  }

  val weather = OpenWeatherMap(10 seconds, 3)_

  /**
   * Decision of tide state by moon position.
   */
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

  def at(date: Date, geoinfo: GeoInfo): Future[Condition] = {
    weather(date, geoinfo) map { weather =>
      val moon = new Moon(date)
      val tide = tideState(geoinfo.longitude, moon.earth_longitude)
      Condition(moon.age.round.toInt, tide, weather)
    }
  }
}
