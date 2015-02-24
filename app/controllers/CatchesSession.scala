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
    imagePath: Option[String] = None)
  object SessionValue {
    implicit val json = Json.format[SessionValue]
  }

  def mkFolder(session: String) = List("photo", Report.Photo.Image.Kind.ORIGINAL, session).mkString("/")

  def start(ticket: String) = Action.async(parse.json(
    (__ \ "geoinfo").readNullable[GeoInfo])
  ) { implicit request =>
    val geoinfo = request.body
    Logger debug f"Starting session by ${ticket} on ${geoinfo}"
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) =>
          val session = SessionValue(ticket.userId, geoinfo)
          val vt = VolatileToken.create(session.asJson, Settings.Session.timeoutUpload)
          Ok(Json.obj(
            "session" -> vt.id,
            "upload" -> Storage.Upload.start(mkFolder(vt.id))
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
        case file :: Nil => session.asToken[SessionValue] match {
          case None => SessionExpired
          case Some((vt, session)) => asPhoto(file) match {
            case None => InternalServerError("Failed to save photo")
            case Some(photo) => vt.copy(content = session.copy(imagePath = Some(photo.original.file.path)).asJson).save match {
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
      session.asToken[SessionValue] match {
        case None => SessionExpired
        case Some((vt, session)) =>
          val ok = for {
            path <- session.imagePath
            image = Storage file path
          } yield {
            val (location, fishes) = InferenceCatches.infer(image, session.geoinfo)
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
      session.asToken[SessionValue] match {
        case None => SessionExpired
        case Some((vt, session)) =>
          val ok = for {
            path <- session.imagePath
            image = Storage file path
          } yield {
            given.copy(userId = session.userId).save match {
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