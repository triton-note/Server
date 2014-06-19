package models

import scala.{Left, Right}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.control.Exception.allCatch

import play.api.Logger
import play.api.Play.current
import play.api.libs.json.JsValue
import play.api.libs.ws.{WS, WSResponse}

object Facebook {
  case class AccessKey(token: String)
  case class ObjectId(id: String)
  object fb {
    val host = "https://graph.facebook.com"
    val client = WS.client
    def /(path: String) = client url "${host}/${path}"
  }
  object parse {
    def JSON(res: WSResponse) = allCatch opt res.json
    def ObjectID(res: WSResponse) = allCatch opt ObjectId((res.json \ "id").as[String])
  }
  object User {
    /**
     * Obtain attributes of user by accessKey.
     * The attributes is specified by fields.
     */
    def obtain(fields: String*)(implicit accesskey: AccessKey): Future[Option[JsValue]] = {
      (fb / "me").withQueryString(
        "fields" -> fields.mkString(","),
        "access_token" -> accesskey.token
      ).get().map(parse.JSON)
    }
    /**
     * Find UserAlias by given accessKey.
     * If accessKey is not valid, return None.
     * If UserAlias is not found, return email which is obtained by accessKey.
     * If UserAlias is found by email, return UserAlias.
     */
    def find(implicit accesskey: AccessKey): Future[Option[Either[String, db.User]]] = {
      obtain("email") map { opt =>
        for {
          json <- opt
          email <- (json \ "email").asOpt[String]
        } yield {
          Logger debug f"Getting UserAlias by email: $email"
          db.Users.find(email) match {
            case Some(user) => Right(user)
            case None       => Left(email)
          }
        }
      }
    }
    /**
     * Create User by email.
     * The email is obtained by accessKey.
     */
    def create(implicit accesskey: AccessKey): Future[Option[db.User]] = {
      obtain("email", "first_name", "last_name", "picture") map { opt =>
        for {
          json <- opt
          email <- (json \ "email").asOpt[String]
          firstName <- (json \ "first_name").asOpt[String]
          lastName <- (json \ "last_name").asOpt[String]
          avatarUrl = (json \ "picture" \ "data" \ "url").asOpt[String]
          user <- db.Users.addNew(email, None, firstName, lastName, avatarUrl)
        } yield {
          Logger.info(f"Creating alias '$email' of $user as facebook and email at once")
          user
        }
      }
    }
    /**
     * Find UserAlias by email which is obtained by accessKey.
     * If UserAlias is not created yet, create it.
     * If accessKey is not valid, return None.
     */
    def apply(accesskey: String): Future[Option[db.User]] = {
      implicit val ak = AccessKey(accesskey)
      Logger.debug(f"Login as user with $ak")
      find flatMap (_ match {
        case Some(e) => e match {
          case Right(user) => Future(Some(user))
          case Left(email) => create
        }
        case None => Future(None)
      })
    }
  }
  object Fishing {
    def publish(photo: List[Storage.S3File], message: Option[String])(implicit accessKey: AccessKey) = {
      (fb / "me/triton-note:fish").withQueryString(
        "access_token" -> accessKey.token
      ).post(Map(
          "image" -> photo.map(_.generateURL(24 hours).toString),
          "message" -> message.toSeq
        )).map(parse.ObjectID)
    }
  }
}
