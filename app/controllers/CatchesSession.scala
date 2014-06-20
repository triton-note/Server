package controllers

import scala.{ Left, Right }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.Exception.allCatch

import scalaz.Scalaz._

import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.{ Action, Controller }

import models.{ GeoInfo, Record, Settings, Storage }
import models.db.{ Users, User, VolatileToken, VolatileTokens }
import service.InferenceCatches

object CatchesSession extends Controller {
  implicit class FoldableForm[T](form: Form[T]) {
    def lr = form.fold(
      (f) => Left(Future(BadRequest(f.errors.mkString("\n")))),
      Right(_))
  }
  object SessionValue {
    implicit val sessionOptionFormat =
      (
        (__ \ "user").format[Option[User]] and
        (__ \ "geoinfo").formatNullable[GeoInfo] and
        (__ \ "photo").formatNullable[Storage.S3File] and
        (__ \ "record").formatNullable[Record]
      )(
          (user, geoinfo, photo, record) => user.map {
            SessionValue(_, geoinfo, photo, record)
          },
          unlift((vOpt: Option[SessionValue]) => vOpt.map {
            v => (Some(v.user), v.geoinfo, v.photo, v.record)
          }))
  }
  case class SessionValue(
    user: User,
    geoinfo: Option[GeoInfo],
    photo: Option[Storage.S3File] = None,
    record: Option[Record] = None) {
    override def toString = Json.toJson(Option(this)).toString
  }
  def start = Action.async { implicit request =>
    val form = Form(tuple(
      "ticket" -> nonEmptyText,
      "geoinfo" -> optional(
        mapping(
          "latitude" -> bigDecimal,
          "longitude" -> bigDecimal
        )(GeoInfo.apply)(GeoInfo.unapply)
      )
    )).bindFromRequest.lr
    form match {
      case Left(res) => res
      case Right((ticket, geoinfo)) => Future {
        val ok = for {
          vt <- VolatileTokens get ticket
          extra <- vt.extra
          user <- (Json.parse(extra) \ "user").as[Option[User]]
        } yield {
          val value = SessionValue(user, geoinfo)
          val session = VolatileTokens.createNew(Settings.Session.timeoutUpload, Option(value.toString))
          Ok(session.id)
        }
        ok getOrElse BadRequest("Ticket Expired")
      }
    }
  }
  def addPhoto = Action.async(parse.multipartFormData) { implicit request =>
    val form = {
      val sessions = request.body.dataParts("session")
      val photoFiles = request.body.files.map(_.ref.file)
      Seq(
        sessions.isEmpty option "session is empty",
        photoFiles.isEmpty option "Photo is not uploaded"
      ).flatten match {
          case Nil    => Right(sessions.head, photoFiles.head)
          case errors => Left(Future(BadRequest(errors.mkString("\n"))))
        }
    }
    form match {
      case Left(res) => res
      case Right((session, photoFile)) => Future {
        val ok = for {
          vt <- VolatileTokens get session
          extra <- vt.extra
          value <- Json.parse(extra).as[Option[SessionValue]]
        } yield {
          val file = Storage.file(value.user.id, session)
          file.write(photoFile)
          vt.setExtra(value.copy(photo = Some(file)).toString)
          val infos = InferenceCatches.infer(file, value.geoinfo)
          val res = Json.arr(infos.map(_.toJson))
          Ok(res.toString)
        }
        ok getOrElse BadRequest("Session Expired")
      }
    }
  }
  def submit = Action.async { implicit request =>
    val form = Form(tuple(
      "session" -> nonEmptyText,
      "record" -> nonEmptyText
    )).bindFromRequest.lr
    form match {
      case Left(res) => res
      case Right((session, json)) => Future {
        val ok = for {
          vt <- VolatileTokens get session
          extra <- vt.extra
          value <- Json.parse(extra).as[Option[SessionValue]]
        } yield {

          NotImplemented
        }
        ok getOrElse BadRequest("Session Expired")
      }
    }
  }
  def publish = Action.async { implicit request =>
    val form = Form(tuple(
      "session" -> nonEmptyText,
      "way" -> nonEmptyText,
      "token" -> nonEmptyText
    )).bindFromRequest.lr
    form match {
      case Left(res) => res
      case Right((session, way, token)) => {
        way match {
          case "facebook" => {
            val ok = for {
              vt <- VolatileTokens get session
              extra <- vt.extra
              value <- Json.parse(extra).as[Option[SessionValue]]
            } yield {

              Future(NotImplemented)
            }
            ok getOrElse Future(BadRequest("Session Expired"))
          }
          case _ => Future(NotImplemented)
        }
      }
    }
  }
}