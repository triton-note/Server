package controllers

import scala.concurrent.Future

import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.{ Action, Controller }

import models.{ GeoInfo, Report, VolatileToken }
import service.{ InferenceCatches, Settings, Storage }

object CatchesSession extends Controller {
  case class SessionValue(
    userId: String,
    geoinfo: Option[GeoInfo],
    imagePath: Option[String] = None) {
  }
  object SessionValue {
    implicit val json = Json.format[SessionValue]
  }
  def mkFolder(session: String) = List("photo", Report.Photo.Kind.ORIGINAL, session).mkString("/")

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
          val session = VolatileToken.create(Json toJson value, Settings.Session.timeoutUpload)
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
          case Some((vt, value, user)) => asPhoto(file) match {
            case None => InternalServerError("Failed to save photo")
            case Some(photo) => vt.copy(data = Json toJson value.copy(imagePath = Some(photo.original.path))).save match {
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
            path <- value.imagePath
            image = Storage file path
          } yield {
            val (location, fishes) = InferenceCatches.infer(image, value.geoinfo)
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
            path <- value.imagePath
            image = Storage file path
          } yield {
            given.copy(userId = user.id).save match {
              case None => InternalServerError(f"Failed to save report: ${given}")
              case Some(saved) =>
                vt.delete
                Ok(saved.id)
            }
          }
          ok getOrElse BadRequest("Any image is not saved yet")
      }
    }
  }
}