package models

import dispatch._
import Defaults._
import play.api.libs.json.Json

object Facebook {
  val host = "https://graph.facebook.com/"
  case class AccessKey(key: String)
  object User {
    def obtain(fields: String*)(implicit accesskey: AccessKey) = {
      val req = url(host + "me").GET << Map(
        "fields" -> fields.mkString(","),
        "access_token" -> accesskey.key)
      Http(req OK as.String).map(Json.parse)
    }
    def apply(accesskey: String): Future[db.UserAlias] = {
      implicit val ak = AccessKey(accesskey)
      obtain("email") flatMap { json =>
        val email = (json \ "email").toString
        db.UserAlias.getByEmail(email) match {
          case Some(user) => Future(user)
          case None => obtain("first_name", "last_name", "picture") map { json =>
            val firstName = (json \ "first_name").toString
            val lastName = (json \ "last_name").toString
            val avatarUrl = (json \ "picture" \ "data" \ "url").asOpt[String]
            val user = db.User.addNew(firstName, lastName, avatarUrl)
            db.UserAlias.addNew(user, email, db.UserAliasDomain.email, 0)
          }
        }
      }
    }
  }
}
