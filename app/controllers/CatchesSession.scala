package controllers

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scalaz.Scalaz._

import play.api.Logger
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{ Action, Controller, Result }

import models.{ GeoInfo, Report, Settings }
import models.db.{ CatchReport, FishSize, Image, Photo, User, VolatileToken }
import service.Facebook.{ AccessKey, Fishing }
import service.InferenceCatches

object CatchesSession extends Controller {
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
    imageId: Option[String] = None,
    report: Option[Report] = None,
    committed: Option[String] = None,
    publishing: Option[SessionValue.Publishing] = None) {
    override def toString = Json.toJson(this).toString
  }

  def start(ticket: String) = Action.async(parse.json(
    (__ \ "geoinfo").readNullable[GeoInfo])
  ) { implicit request =>
    val geoinfo = request.body
    Logger debug f"Starting session by ${ticket} on ${geoinfo}"
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => BadRequest("Ticket Expired")
        case Some((vt, _, user)) =>
          val value = SessionValue(user.id, geoinfo)
          val session = VolatileToken.addNew(Settings.Session.timeoutUpload, Option(value.toString))
          Ok(session.id)
      }
    }
  }

  def photo(session: String) = Action.async(parse.multipartFormData) { implicit request =>
    val files = request.body.files
    Logger debug f"Saving head of ${files}"
    files.headOption.map(_.ref.file) match {
      case None => Future(BadRequest("No uploaded files"))
      case Some(file) => Future(file.asPhoto).flatMap {
        _ match {
          case None => Future(InternalServerError("Failed to save photo"))
          case Some(photo) =>
            mayCommit(session) {
              _.copy(imageId = Some(photo.original.id))
            } { value =>
              value.report.isEmpty option Ok {
                val (location, fishes) = InferenceCatches.infer(photo.original.file, value.geoinfo)
                Json.obj(
                  "url" -> photo.asURL,
                  "location" -> location,
                  "fishes" -> fishes
                )
              }
            }
        }
      }
    }
  }

  def submit(session: String) = Action.async(parse.json((
    (__ \ "report").read[Report] and
    (__ \ "publishing").readNullable[SessionValue.Publishing]
  ).tupled)) { implicit request =>
    val (report, publishing) = request.body
    Logger debug f"Sumit report with publishing $publishing"
    mayCommit(session) {
      _.copy(report = Some(report), publishing = publishing)
    } { _.imageId.isEmpty option Ok }
  }

  def mayCommit(token: String)(convert: SessionValue => SessionValue)(result: SessionValue => Option[Result]): Future[Result] = {
    def commit(vt: VolatileToken)(implicit user: User): Future[Result] = {
      val ok = for {
        value <- vt.json[SessionValue]
        given <- value.report
        image <- value.imageId.flatMap(Image.get)
      } yield {
        val report = CatchReport.addNew(user, given.location.geoinfo, given.location.name, given.dateAt)
        val photo = Photo.addNew(report, image)
        val comment = report.addComment(given.comment)
        val photos = given.fishes.map(_ add photo)
        vt json value.copy(committed = Some(report.id)) match {
          case Some(vt) => value.publishing match {
            case Some(SessionValue.Publishing(way, token)) => publish(way, token)(given, image)
            case None                                      => Future(Ok)
          }
          case None => Future(InternalServerError("Failed to update the session"))
        }
      }
      ok getOrElse Future(InternalServerError("Failed to commit the submit"))
    }
    @tailrec
    def retry(count: Int): Future[Result] = token.asTokenOfUser[SessionValue] match {
      case None => Future(BadRequest("Session Expired"))
      case Some((vt, value, user)) =>
        val next = convert(value)
        vt json next match {
          case Some(vt)          => result(next).map(Future(_)) getOrElse commit(vt)(user)
          case None if count > 0 => retry(count - 1)
          case None              => Future(InternalServerError("Failed to update the session"))
        }
    }
    retry(3)
  }
  def publish(way: String, token: String)(report: Report, image: Image): Future[Result] = {
    def mkMessage = {
      val catches = report.fishes.map { fish =>
        val size = List(
          fish.length.map { v => f"${v.value} ${v.unit}" },
          fish.weight.map { v => f"${v.value} ${v.unit}" }
        ).flatten match {
            case Nil  => ""
            case list => list.mkString("(", ",", ")")
          }
        f"${fish.name}${size} x ${fish.count}"
      }.mkString("\n")
      report.comment + "\n\n" + catches
    }
    way match {
      case "facebook" =>
        Fishing.publish(List(image), Some(mkMessage))(new AccessKey(token)).map(_ match {
          case Some(id) => Ok
          case None     => InternalServerError(f"Failed to publish to $way")
        })
      case _ => Future(NotImplemented(f"No way for Publishing '${way}'"))
    }
  }
}