package models

import play.api.libs.functional.syntax._
import play.api.libs.json._

import org.fathens.math._

/**
 * Hold geographic location, latitude and longitude, in degrees.
 */
case class GeoInfo(latitude: Degrees, longitude: Degrees)
object GeoInfo {
  implicit val json: Format[GeoInfo] = {
    def degrees(p: JsPath) = p.format[Double].inmap(Degrees.apply, unlift(Degrees.unapply))
    (
      degrees(__ \ "latitude") and
      degrees(__ \ "longitude")
    )(GeoInfo.apply, unlift(GeoInfo.unapply))
  }
}
