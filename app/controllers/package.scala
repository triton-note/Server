import scala.util.control.Exception.allCatch

import play.api.libs.json._

import models.db.{Users, VolatileTokens}

package object controllers {
  object TicketValue {
    implicit val ticketFormat = Json.format[TicketValue]
  }
  case class TicketValue(
    userId: String,
    way: String,
    token: String) {
    override def toString = Json.toJson(this).toString
  }
  /**
   * 文字列を token として扱い、ユーザIDが含まれている事を確認する拡張
   */
  implicit class TokenOfUser(token: String) {
    def asTokenOfUser[T <: { val userId: String }](implicit reads: Reads[T]) = for {
      vt <- VolatileTokens get token
      extra <- vt.extra
      json <- allCatch opt Json.parse(extra)
      value <- json.asOpt[T]
      user <- Users get value.userId
    } yield (vt, value, user)
  }
}
