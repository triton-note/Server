package models

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper

import org.fathens.math._

/**
 * Hold geographic location, latitude and longitude, in degrees.
 */
case class GeoInfo(latitude: Degrees, longitude: Degrees)
object GeoInfo {
  implicit val degreesFormat: Format[Degrees] = __.format[Double].inmap(Degrees.apply, unlift(Degrees.unapply))
  implicit val geoinfoFormat = Json.format[GeoInfo]
}
