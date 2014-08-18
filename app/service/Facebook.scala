package service

import scala.{ Left, Right }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api.Logger
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.ws.{ WS, WSResponse }
import play.api.mvc.Codec.utf_8

import org.fathens.play.util.Exception.allCatch

import models.{ Profile, Settings }
import models.db.{ Image, SocialConnection, User => UserDB }

object Facebook {
  case class AccessKey(token: String)
  case class ObjectId(id: String)
  object fb {
    lazy val host = System.getenv("FACEBOOK_HOST")
    val client = WS.client
    def /(path: String) = client url f"${host}/${path}"
  }
  object parse {
    def JSON(res: WSResponse) = res.status match {
      case 200 => allCatch opt res.json
      case _ =>
        Logger error f"Status(${res.status}) ${res.body}"
        None
    }
    def ObjectID(res: WSResponse) = for {
      json <- JSON(res)
      id <- (json \ "id").asOpt[String]
    } yield ObjectId(id)
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
     * If User is not found, return accountId which is obtained by accessKey.
     * If User is found by accountId, return User.
     */
    def find(implicit accesskey: AccessKey): Future[Option[Either[String, UserDB]]] = {
      obtain("email") map { opt =>
        for {
          json <- opt
          id <- (json \ "id").asOpt[String]
        } yield {
          Logger debug f"Getting User of facebook by id: ${id}"
          SocialConnection.findBy(id, SocialConnection.Service.FACEBOOK).flatMap(_.user) match {
            case Some(user) => Right(user)
            case None       => Left(id)
          }
        }
      }
    }
    /**
     * Create User by email.
     * The email is obtained by accessKey.
     */
    def create(implicit accesskey: AccessKey): Future[Option[UserDB]] = {
      obtain("email", "name", "picture") map { opt =>
        for {
          json <- opt
          id <- (json \ "id").asOpt[String]
          email <- (json \ "email").asOpt[String]
          name <- (json \ "name").asOpt[String]
          avatarUrl = (json \ "picture" \ "data" \ "url").asOpt[String]
        } yield {
          val user = UserDB.addNew(email, name, avatarUrl)
          val social = SocialConnection.addNew(id, SocialConnection.Service.FACEBOOK, user)
          Logger.info(f"Creating ${user} as ${social}")
          user
        }
      }
    }
    def connect(user: UserDB)(implicit accesskey: AccessKey): Future[Option[SocialConnection]] = {
      obtain("id") map { opt =>
        for {
          json <- opt
          id <- (json \ "id").asOpt[String]
        } yield {
          val social = SocialConnection.addNew(id, SocialConnection.Service.FACEBOOK, user)
          Logger.info(f"Connecting ${user} to ${social}")
          social
        }
      }
    }
    def profile(implicit accesskey: AccessKey): Future[Option[Profile]] = {
      obtain("email", "name", "picture") map { opt =>
        for {
          json <- opt
          email <- (json \ "email").asOpt[String]
          name <- (json \ "name").asOpt[String]
          avatarUrl = (json \ "picture" \ "data" \ "url").asOpt[String]
        } yield Profile(
          email,
          name,
          avatarUrl
        )
      }
    }
    /**
     * Find UserAlias by email which is obtained by accessKey.
     * If UserAlias is not created yet, create it.
     * If accessKey is not valid, return None.
     */
    def apply(implicit accesskey: AccessKey): Future[Option[UserDB]] = {
      Logger.debug(f"Login as user with ${accesskey}")
      find flatMap (_ match {
        case Some(e) => e match {
          case Right(user) => Future(Some(user))
          case Left(id)    => create
        }
        case None => Future(None)
      })
    }
  }
  object Fishing {
    def publish(photo: List[Image], message: Option[String])(implicit accessKey: AccessKey): Future[Option[ObjectId]] = {
      (fb / f"me/photos").withQueryString(
        "access_token" -> accessKey.token
      ).post(Map(
          "url" -> photo.map(_.url(Settings.Pulish.timer).toString),
          "message" -> message.toSeq
        )).map(parse.ObjectID)
    }
  }
}
