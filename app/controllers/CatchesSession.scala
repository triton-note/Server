package controllers

import scala.concurrent.Future

import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.{ Action, Controller }

import models.{ GeoInfo, Report }
import models.db.{ CatchReport, Image, Photo, VolatileToken }
import service.{ InferenceCatches, Settings, Storage }

object CatchesSession extends Controller {
  object SessionValue {
    implicit val sessionOptionFormat = Json.format[SessionValue]
  }
  case class SessionValue(
    userId: String,
    geoinfo: Option[GeoInfo],
    imageId: Option[String] = None,
    committed: Option[String] = None) {
    override def toString = Json.toJson(this).toString
  }
  def mkFolder(session: String) = List("photo", Image.Kind.ORIGINAL.toString, session).mkString("/")

  def start(ticket: String) = Action.async(parse.json(
    (__ \ "geoinfo").readNullable[GeoInfo])
  ) { implicit request =>
    val geoinfo = request.body
    Logger debug f"Starting session by ${ticket} on ${geoinfo}"
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => TicketExpired
        case Some((vt, _, user)) =>
          val value = SessionValue(user.id, geoinfo)
          val session = VolatileToken.addNew(Settings.Session.timeoutUpload, Option(value.toString))
          Ok(Json.obj(
            "session" -> session.id,
            "upload" -> Storage.Upload.start(mkFolder(session.id))
          ))
      }
    }
  }

  def photo(session: String) = Action.async(parse.json(
    (__ \ "names").read[Set[String]]
  )) { implicit request =>
    val files = request.body.map(Storage.file(mkFolder(session), _))
    Logger debug f"Saving head of ${files}"
    Future {
      files.toList match {
        case Nil => BadRequest("No uploaded files")
        case file :: Nil => session.asTokenOfUser[SessionValue] match {
          case None => SessionExpired
          case Some((vt, value, user)) => file.asPhoto match {
            case None => InternalServerError("Failed to save photo")
            case Some(photo) => vt json value.copy(imageId = Some(photo.original.path)) match {
              case None => InternalServerError("Failed to save session value")
              case Some(_) => Ok(Json.obj(
                "url" -> photo
              ))
            }
          }
        }
        case _ => BadRequest("Too much uploaded files")
      }
    }
  }

  def infer(session: String) = Action.async { implicit request =>
    Logger debug f"Inferring of photo on session: ${session}"
    Future {
      session.asTokenOfUser[SessionValue] match {
        case None => SessionExpired
        case Some((vt, value, user)) =>
          val ok = for {
            imageId <- value.imageId
            image <- Image get imageId
          } yield {
            val (location, fishes) = InferenceCatches.infer(image.file, value.geoinfo)
            Ok(Json.obj(
              "location" -> location,
              "fishes" -> fishes
            ))
          }
          ok getOrElse BadRequest("Any image is not saved yet")
      }
    }
  }

  def submit(session: String) = Action.async(parse.json(
    (__ \ "report").read[Report]
  )) { implicit request =>
    val given = request.body
    Logger debug f"Sumit report: ${given}"
    Future {
      session.asTokenOfUser[SessionValue] match {
        case None => SessionExpired
        case Some((vt, value, user)) =>
          val ok = for {
            imageId <- value.imageId
            image <- Image get imageId
          } yield {
            val report = CatchReport.addNew(user, given.location.geoinfo, given.location.name, given.dateAt, given.condition)
            val photo = Photo.addNew(report, image)
            val comment = report.addComment(given.comment)(user)
            val photos = given.fishes.map(_ add photo)
            vt json value.copy(committed = Some(report.id)) match {
              case None    => InternalServerError("Failed to save session value")
              case Some(_) => Ok(report.id)
            }
          }
          ok getOrElse BadRequest("Any image is not saved yet")
      }
    }
  }
}