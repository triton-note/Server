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
  }
}
class TideMoon(val date: java.util.Date, val geoinfo: GeoInfo) {
  import TideMoon._

  val moon = new Moon(date)

  val state = {
    val diff: Degrees = (moon.earth_longitude - geoinfo.longitude + Degrees(15)).normalize
    (diff % Pi).toDouble match {
      case d if d < 30             => TideState.High
      case d if 30 <= d && d <= 90 => TideState.Ebb
      case d if 90 < d && d < 120  => TideState.Low
      case d if 120 <= d           => TideState.Flood
    }
  }
}
