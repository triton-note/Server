package service

import scala.concurrent.Future

import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.ws.{ WS, WSResponse }
import play.api.mvc.Codec.utf_8
import play.api.mvc.RequestHeader

import org.fathens.play.util.Exception.allCatch

import models.Report

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
