package models

import play.api.libs.json._

case class User(name: String, measureUnit: ValueUnit.Measures, connections: User.SocialConnection)
object User {
  case class SocialConnection(service: SocialConnection.Service.Value, accountId: String, connected: Boolean)
  object SocialConnection {
    object Service extends Enumeration {
      val FACEBOOK = Value("facebook")
      val GOOGLE = Value("google")
      implicit val json = Format[Service.Value](
        (__).read[String].map(Service.withName),
        Writes { Json toJson _.toString })
    }
    implicit val json = Json.format[SocialConnection]
  }
  implicit val json = Json.format[User]
}
