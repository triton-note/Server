package models

import play.Logger
import dispatch._
import Defaults._
import play.api.libs.json.Json

object Facebook {
  val host = "https://graph.facebook.com/"
  case class AccessKey(token: String)
  object User {
    def obtain(fields: String*)(implicit accesskey: AccessKey) = {
      val req = url(host + "me").GET << Map(
        "fields" -> fields.mkString(","),
        "access_token" -> accesskey.token)
      Http(req OK as.String).map(Json.parse)
    }
    def find(implicit accesskey: AccessKey): Future[Option[Either[String, db.UserAlias]]] = {
      obtain("email") map { json =>
        for {
          email <- (json \ "email").asOpt[String]
        } yield db.UserAlias.get(email, db.UserAliasDomain.facebook) match {
          case Some(user) => Right(user)
          case None       => Left(email)
        }
      }
    }
    def create(implicit accesskey: AccessKey): Future[Option[db.UserAlias]] = {
      obtain("email", "first_name", "last_name", "picture") map { json =>
        for {
          email <- (json \ "email").asOpt[String]
          firstName <- (json \ "first_name").asOpt[String]
          lastName <- (json \ "last_name").asOpt[String]
        } yield {
          val avatarUrl = (json \ "picture" \ "data" \ "url").asOpt[String]
          val user = db.User.addNew(firstName, lastName, avatarUrl)
          Logger.info(f"Creating alias '$email' of $user as facebook and email at once")
          def as(f: db.UserAliasDomain.type => String) = db.UserAlias.addNew(user, email, f(db.UserAliasDomain), 0)
          as(_.email)
          as(_.facebook)
        }
      }
    }
    def apply(accesskey: String): Future[Option[db.UserAlias]] = {
      implicit val ak = AccessKey(accesskey)
      find flatMap {
        _ match {
          case Some(e) => e match {
            case Right(user) => Future(Some(user))
            case Left(email) => create
          }
          case None => Future(None)
        }
      }
    }
  }
}
