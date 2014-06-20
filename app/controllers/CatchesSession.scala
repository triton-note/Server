package controllers

import scala.{Left, Right}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.Exception.allCatch

import scalaz.Scalaz._

import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.{Action, Controller}

import models.{GeoInfo, Record, Settings, Storage}
import models.db.{User, Users, VolatileToken, VolatileTokens}
import service.InferenceCatches

object CatchesSession extends Controller {
  implicit class FoldableForm[T](form: Form[T]) {
    def lr = form.fold(
      (f) => Left(Future(BadRequest(f.errors.mkString("\n")))),
      Right(_))
  }
  def getUser(vt: VolatileToken) = {
    for {
      json <- vt.extra
      id <- (Json.parse(json) \ "user").asOpt[String]
      user <- Users get id
    } yield user
  }
  def toJson(user: User, geoinfo: Option[GeoInfo], photo: Option[Storage.S3File] = None, record: Option[Record] = None) = Json.obj(
    "user" -> user.id,
    "geoinfo" -> geoinfo,
    "photo" -> photo,
    "record" -> record
  )
  def fromToken(token: String) = {
    for {
      vt <- VolatileTokens get token
      extra <- vt.extra
      top <- allCatch opt Json.parse(extra)
      userId <- (top \ "user").asOpt[String]
      user <- Users get userId
    } yield {
      val geoinfo = (top \ "geoinfo").asOpt[GeoInfo]
      val photo = (top \ "photo").asOpt[Storage.S3File]
      val record = (top \ "catches").asOpt[Record]
      (vt, user, geoinfo, photo, record)
    }
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
        fromToken(ticket).map {
          case (_, user, _, _, _) =>
            val value = toJson(user, geoinfo)
            val ticket = VolatileTokens.createNew(Settings.Session.timeoutUpload, Option(value.toString))
            Ok(ticket.id)
        } getOrElse BadRequest("Ticket Expired")
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
        fromToken(session).map {
          case (vt, user, geoinfo, _, catches) =>
            val file = Storage.file(user.id, session)
            file.write(photoFile)
            vt.setExtra(toJson(user, geoinfo, Option(file), catches))
            val infos = InferenceCatches.infer(file, geoinfo)
            val res = Json.arr(infos.map(_.toJson))
            Ok(res.toString)
        } getOrElse BadRequest("Session Expired")
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
        fromToken(session).map {
          case (vt, user, geoinfo, photo, _) =>

            NotImplemented
        } getOrElse BadRequest("Session Expired")
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
            fromToken(session).map {
              case (vt, user, geoinfo, photo, record) =>

                Future(NotImplemented)
            } getOrElse Future(BadRequest("Session Expired"))
          }
          case _ => Future(NotImplemented)
        }
      }
    }
  }
}