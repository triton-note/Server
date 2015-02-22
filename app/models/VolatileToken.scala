package models

import java.util.Date

import scala.concurrent.duration._

import play.api.libs.json._

case class VolatileToken(id: String, expiration: Date, data: JsValue)
object VolatileToken {
  implicit val json = Json.format[VolatileToken]
  
  def create(data: JsValue, period: FiniteDuration) = {
    val id = play.api.libs.Crypto.generateToken
    val expiration = new Date(new Date().getTime + period.toMillis)
    VolatileToken(id, expiration, data)
  }
}
