import play.api.libs.json._

import org.fathens.play.util.Exception.allCatch

import models.db.{Users, VolatileToken, VolatileTokens}

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
   * Token に紐付けして保存してある情報を JSON と読み書きする
   */
  implicit class VolatileJson(vt: VolatileToken) {
    def json[T](implicit reads: Reads[T]): Option[T] = for {
      extra <- vt.extra
      json <- allCatch opt Json.parse(extra)
      value <- json.asOpt[T]
    } yield value
    def json[T](value: T)(implicit writes: Writes[T]): Option[VolatileToken] = for {
      json <- allCatch opt Json.toJson(value)
      next <- vt setExtra json.toString
    } yield next
  }
  /**
   * 文字列を token として扱い、ユーザIDが含まれている事を確認する拡張
   */
  implicit class TokenOfUser(token: String) {
    def asTokenOfUser[T <: { val userId: String }](implicit reads: Reads[T]) = for {
      vt <- VolatileTokens get token
      value <- vt.json[T]
      user <- Users get value.userId
    } yield (vt, value, user)
  }
}
