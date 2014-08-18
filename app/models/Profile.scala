package models

import play.api.libs.json._

case class Profile(email: String, name: String, avatar: Option[String])
object Profile {
    implicit val profileFormat = Json.format[Profile]
}