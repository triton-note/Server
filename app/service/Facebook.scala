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

import models.{ Report, User => Account, ValueUnit }

object Facebook {
  case class AccessKey(token: String)
  case class ObjectId(id: String)
  object fb {
    lazy val host = settings.facebook.host
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
    def find(implicit accesskey: AccessKey): Future[Option[Either[String, Account]]] = {
      obtain("id") map { opt =>
        for {
          json <- opt
          id <- (json \ "id").asOpt[String]
        } yield {
          Logger debug f"Getting User of facebook by id: ${id}"
          Account.findBy(_.FACEBOOK, id, true) match {
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
    def create(implicit accesskey: AccessKey): Future[Option[Account]] = {
      obtain("id", "name") map { opt =>
        for {
          json <- opt
          id <- (json \ "id").asOpt[String]
          name <- (json \ "name").asOpt[String]
        } yield {
          val user = Account.create(name,
            ValueUnit.Measures(ValueUnit.Length.Measure.CM, ValueUnit.Weight.Measure.KG, ValueUnit.Temperature.Measure.Cels),
            Account.SocialConnection.of(_.FACEBOOK, id, true))
          Logger.info(f"Creating ${user}")
          user
        }
      }
    }
    def connect(user: Account)(implicit accesskey: AccessKey): Future[Option[Account]] = {
      obtain("id") map { opt =>
        for {
          json <- opt
          id <- (json \ "id").asOpt[String]
          saved <- user.connect(Account.SocialConnection.Service.FACEBOOK, id)
        } yield saved
      }
    }
    /**
     * Find User by email which is obtained by accessKey.
     * If User is not created yet, create it.
     * If accessKey is not valid, return None.
     */
    def apply(implicit accesskey: String): Future[Option[Account]] = {
      implicit val key = AccessKey(accesskey)
      Logger.debug(f"Login as user with ${accesskey}")
      find flatMap (_ match {
        case None => Future(None)
        case Some(e) => e match {
          case Right(user) => Future(Some(user))
          case Left(id) => Account.findBy(_.FACEBOOK, id, false) match {
            case None       => create
            case Some(user) => connect(user)
          }
        }
      })
    }
  }
  object Report {
    def publish(report: Report)(implicit accessKey: AccessKey, request: RequestHeader): Future[Option[ObjectId]] = {
      def model(f: controllers.routes.ModelView.type => play.api.mvc.Call) =
        Seq(f(controllers.routes.ModelView).absoluteURL(true))
      val appName = settings.facebook.appName
      val actionName = settings.facebook.publish.actionName
      val objectName = settings.facebook.publish.objectName
      val params = {
        Map(
          "fb:explicitly_shared" -> Seq("true"),
          "message" -> report.comment.toSeq,
          "place" -> model(_ spot report.id),
          objectName -> model(_ catchReport report.id)
        ) ++ {
            for {
              (photo, index) <- report.photo.zipWithIndex
              (key, value) <- Map(
                "url" -> photo.original.file.generateURL(settings.facebook.publish.imageTimeout),
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
