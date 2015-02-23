package models

import java.util.Date

import scala.concurrent.duration._

import play.api.libs.json._

case class VolatileToken(id: String, expiration: Date, data: JsValue) {
  def save: Option[VolatileToken] = VolatileToken.save(this)
  def delete = VolatileToken.delete(id)
}
object VolatileToken {
  implicit val json = Json.format[VolatileToken]
  val tableName = "VOLATILE_TOKEN"

  /**
   *  Connect to DynamoDB Table
   */
  lazy val DB = new TableDelegate("VOLATILE_TOKEN")

  def create(data: JsValue, period: FiniteDuration) = {
    val expiration = new Date(new Date().getTime + period.toMillis)
    VolatileToken(generateId, expiration, data).save.get
  }
  def get(id: String): Option[VolatileToken] = DB get id
  def save(vt: VolatileToken): Option[VolatileToken] = DB save vt
  def delete(id: String): Boolean = DB delete id
  def deleteExpired: Int = {
    0
  }
}
