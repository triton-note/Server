package controllers

import scala.{Left, Right}
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scalaz.Scalaz._

import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json._
import play.api.mvc.{Action, Controller}

import models.{GeoInfo, Record, Settings}
import models.db.{CatchReport, CatchReports, FishSizes, Image, Images, Photos, User, Users, VolatileTokens}
import service.InferenceCatches

object CatchesSession extends Controller {
  implicit class FoldableForm[T](form: Form[T]) {
    def lr = form.fold(
      (f) => Left(Future(BadRequest(f.errors.mkString("\n")))),
      Right(_))
  }
  object SessionValue {
    case class Publishing(way: String, token: String)
    object Publishing {
      implicit val publishingFormat = Json.format[Publishing]
    }
    implicit val sessionOptionFormat = Json.format[SessionValue]
  }
  case class SessionValue(
    userId: String,
    geoinfo: Option[GeoInfo],
    imageId: Option[Long] = None,
    record: Option[Record] = None,
    committed: Option[Long] = None,
    publishing: Option[SessionValue.Publishing] = None) {
    lazy val user = Users get userId
    lazy val image = imageId.flatMap(Images.get)
    override def toString = Json.toJson(this).toString
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
          userId <- (Json.parse(extra) \ "user").asOpt[String]
          user <- Users get userId
        } yield {
          val value = SessionValue(user.id, geoinfo)
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
      case Right((session, file)) => Future {
        val res = for {
          vt <- VolatileTokens get session
          extra <- vt.extra
          value <- Json.parse(extra).asOpt[SessionValue]
          user <- value.user
        } yield {
          Images.addNew(file) match {
            case Some(image) => {
              value.record match {
                case None => {
                  vt setExtra value.copy(imageId = Some(image.id)).toString
                  val catches = InferenceCatches.infer(image.file, value.geoinfo)
                  Ok(Json toJson catches)
                }
                case Some(record) => {
                  commit(image, record)(user) match {
                    case Some(report) => {
                      vt setExtra value.copy(committed = Some(report.id)).toString
                      Ok(Json toJson record.catches)
                    }
                    case None => InternalServerError("Failed to commit the submit")
                  }
                }
              }
            }
            case None => InternalServerError("Failed to save photo")
          }
        }
        res getOrElse BadRequest("Session Expired")
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
        val res = for {
          vt <- VolatileTokens get session
          extra <- vt.extra
          value <- Json.parse(extra).asOpt[SessionValue]
          user <- value.user
        } yield {
          val given = Json.parse(json).as[Record]
          value.image match {
            case Some(image) => commit(image, given)(user) match {
              case Some(report) => {
                vt setExtra value.copy(committed = Some(report.id)).toString
                Ok
              }
              case None => InternalServerError("Failed to commit the submit")
            }
            case None => {
              vt setExtra value.copy(record = Some(given)).toString
              Ok
            }
          }
        }
        res getOrElse BadRequest("Session Expired")
      }
    }
  }
  def commit(image: Image, given: Record)(implicit user: User): Option[CatchReport] = {
    for {
      report <- CatchReports.addNew(user, given.geoinfo.get, given.date)
      photo <- Photos.addNew(report, image)
    } yield {
      given.catches.map { fish =>
        FishSizes.addNew(photo, fish.name, fish.count, fish.weight, fish.length)
      }
      report.addComment(given.comment)
      report
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