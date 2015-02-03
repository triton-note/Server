package service

import java.util.Date

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import org.fathens.astronomy.Moon
import org.fathens.math._

import models.GeoInfo
import models.Report.Condition.Tide
import models.Report.Condition.Weather
import play.api.Logger

object NaturalConditions {
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
}
class NaturalConditions(val date: java.util.Date, val geoinfo: GeoInfo) {
  import NaturalConditions._

  val moon = new Moon(date)

  val tide = tideState(geoinfo.longitude, moon.earth_longitude)

  val weather: Future[Weather] = Future {
    Weather("Fine", 20.0)
  }
}
