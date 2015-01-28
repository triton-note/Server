package service

import org.fathens.astronomy.Moon
import org.fathens.math._

import models.GeoInfo

object TideMoon {
  object TideState extends Enumeration {
    val High = Value("High")
    val Ebb = Value("Ebb")
    val Low = Value("Low")
    val Flood = Value("Flood")

    def of(origin: Degrees, moon: Degrees): Value = {
      def anguler: Degrees = {
        val diff = moon - origin + Degrees(15)
        diff.normalize % Pi
      }
      anguler.toDouble match {
        case d if d < 30             => TideState.High
        case d if 30 <= d && d <= 90 => TideState.Flood
        case d if 90 < d && d < 120  => TideState.Low
        case d if 120 <= d           => TideState.Ebb
      }
    }
  }
}
class TideMoon(val date: java.util.Date, val geoinfo: GeoInfo) {
  import TideMoon._

  val moon = new Moon(date)

  val state = TideState.of(geoinfo.longitude, moon.earth_longitude)
}
