package models

import java.util.Date

import scala.concurrent.duration._

import play.api.libs.json._

case class VolatileToken(id: String, expiration: Date, data: JsValue) {
  def save: Option[VolatileToken] = {
    Option(this)
  }
  def delete = {
    
  }
}
object VolatileToken {
  implicit val json = Json.format[VolatileToken]
  val tableName = "VOLATILE_TOKEN"
  
  def create(data: JsValue, period: FiniteDuration) = {
    val expiration = new Date(new Date().getTime + period.toMillis)
    VolatileToken(generateId, expiration, data).save.get
  }
  def get(id: String): Option[VolatileToken] = {
    None
  }
  def deleteExpired: Int = {
    0
  }
}
