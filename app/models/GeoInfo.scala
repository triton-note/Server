package models

import scala.math.{BigDecimal, cos, pow, sin, sqrt}

import play.api.libs.json._

/**
 * Hold geographic location, latitude and longitude, in degrees.
 */
case class GeoInfo(latitude: Double, longitude: Double) {
  import GeoInfo._
  object radian {
    lazy val lat = latitude.toDouble.toRadians
    lazy val lng = longitude.toDouble.toRadians
  }
}
object GeoInfo {
  implicit val geoinfoFormat = Json.format[GeoInfo]
}
