package service

import scala.{ Left, Right }
import scala.concurrent.Future

import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.libs.ws.{ WS, WSResponse }
import play.api.mvc.Codec.utf_8
import play.api.mvc.RequestHeader

import org.fathens.play.util.Exception.allCatch

import models.db.{ CatchReport, Photo, SocialConnection, User => UserDB }

object Facebook {
  case class AccessKey(token: String)
  case class ObjectId(id: String)
  lazy val appName = Settings.FACEBOOK_APP_NAME
  lazy val actionName = Settings.FACEBOOK_CATCH_ACTION
  lazy val objectName = Settings.FACEBOOK_CATCH_OBJECT
  object fb {
    lazy val host = Settings.FACEBOOK_HOST
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
     * Find User by given accessKey.
     * If accessKey is not valid, return None.
     * If User is not found, return accountId which is obtained by accessKey.
     * If User is found by accountId, return User.
     */
    def find(implicit accesskey: AccessKey): Future[Option[Either[String, UserDB]]] = {
      obtain("id") map { opt =>
        for {
          json <- opt
          id <- (json \ "id").asOpt[String]
        } yield {
          Logger debug f"Getting User of facebook by id: ${id}"
          SocialConnection.findBy(id, SocialConnection.Service.FACEBOOK).flatMap(_.connect).flatMap(_.user) match {
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
      obtain("id", "name") map { opt =>
        for {
          json <- opt
          id <- (json \ "id").asOpt[String]
          name <- (json \ "name").asOpt[String]
        } yield {
          val user = UserDB.addNew(name)
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
          social <- SocialConnection.findBy(id, SocialConnection.Service.FACEBOOK) match {
            case Some(social) => social.connect
            case None         => Option(SocialConnection.addNew(id, SocialConnection.Service.FACEBOOK, user))
          }
        } yield social
      }
    }
    /**
     * Find User by email which is obtained by accessKey.
     * If User is not created yet, create it.
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
  object Report {
    def publish(report: CatchReport)(implicit accessKey: AccessKey, request: RequestHeader): Future[Option[ObjectId]] = {
      def model(f: controllers.routes.ModelView.type => play.api.mvc.Call) =
        Seq(f(controllers.routes.ModelView).absoluteURL(request.secure))
      val params = {
        val images = Photo.findBy(report).flatMap(_.image)
        Map(
          "fb:explicitly_shared" -> Seq("true"),
          "message" -> report.topComment.map(_.text).toSeq,
          "place" -> model(_ spot report.id),
          objectName -> model(_ catchReport report.id)
        ) ++ {
            for {
              (image, index) <- images.zipWithIndex
              (key, value) <- Map(
                "url" -> image.url(Settings.Pulish.timer),
                "user_generated" -> true
              )
            } yield f"image[${index}][${key}]" -> Seq(value.toString)
          }
      }
      (fb / f"me/${appName}:${actionName}").withQueryString(
        "access_token" -> accessKey.token
      ).post(params).map(parse.ObjectID)
    }
  }
}
