import play.api.libs.json._
import play.api.mvc.Results.BadRequest

import models.VolatileToken

package object controllers {
  case class TicketValue(userId: String)
  object TicketValue {
    implicit val json = Json.format[TicketValue]
  }
  val TicketExpired = BadRequest("Ticket Expired")

  /**
   * 文字列を token として扱い、ユーザIDが含まれている事を確認する拡張
   */
  implicit class TokenOfUser(token: String) {
    def asToken[T <: { val userId: String }](implicit reads: Reads[T]) = for {
      vt <- VolatileToken get token
      value <- vt.data.asOpt[T]
    } yield (vt, value)
  }

  implicit class AsJson[J](o: J)(implicit $writer: Writes[J]) {
    def asJson = Json toJson o
  }
}
