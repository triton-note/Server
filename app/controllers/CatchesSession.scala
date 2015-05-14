package controllers

import scala.concurrent.Future

import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{ Action, Controller }

import models.{ GeoInfo, Photo, Report, User, VolatileToken }
import service.{ InferenceCatches, Storage }

object CatchesSession extends Controller {
  case class SessionValue(
    userId: String,
    geoinfo: Option[GeoInfo],
    imagePath: Option[String] = None) {

    /**
     * delete files that associated with this session
     */
    def delete = imagePath.foreach { path =>
      val original = Storage.file(path)
      val reduced = Photo.Image.reduced(original)
      (original :: reduced).par.foreach(_.delete)
    }
  }
  object SessionValue {
    implicit val json = Json.format[SessionValue]
  }
  val SessionExpired = BadRequest("Session Expired")

  def start = Action.async(parse.json((
    (__ \ "ticket").read[String] and
    (__ \ "geoinfo").readNullable[GeoInfo]
  ).tupled)) { implicit request =>
    val (ticket, geoinfo) = request.body
    Logger debug f"Starting session by ${ticket} on ${geoinfo}"
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) => User.get(ticket.userId) match {
          case None => Unauthorized
          case Some(user) =>
            val session = SessionValue(ticket.userId, geoinfo)
            val vt = VolatileToken.create(session.asJson, settings.token.session)
            val folder = Photo.Image.file(_.ORIGINAL)(user.cognitoId, vt.id).path
            Ok(Json.obj(
              "session" -> vt.id,
              "upload" -> Storage.Upload.start(folder)
            ))
        }
      }
    }
  }

  def photo = Action.async(parse.json((
    (__ \ "session").read[String] and
    (__ \ "names").read[Set[String]]
  ).tupled)) { implicit request =>
    val (session, names) = request.body
    Logger debug f"Saving head of ${names}"
    Future {
      session.asToken[SessionValue] match {
        case None => SessionExpired
        case Some((vt, session)) => User.get(session.userId) match {
          case None => Unauthorized
          case Some(user) =>
            names.toList match {
              case Nil => BadRequest("No uploaded files")
              case name :: Nil => Photo.of(user, vt.id, name) match {
                case None => InternalServerError("Failed to save photo")
                case Some(photo) => vt.copy(data = session.copy(imagePath = Some(photo.original.file.path)).asJson).save match {
                  case None => InternalServerError("Failed to save session value")
                  case Some(_) => Ok(Json.obj(
                    "url" -> photo
                  ))
                }
              }
              case _ => BadRequest("Too much uploaded files")
            }
        }
      }
    }
  }

  def infer = Action.async(parse.json(
    (__ \ "session").read[String]
  )) { implicit request =>
    val session = request.body
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

  def submit = Action.async(parse.json((
    (__ \ "session").read[String] and
    (__ \ "report").read[Report]
  ).tupled)) { implicit request =>
    val (session, given) = request.body
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
                vt.copy(data = "".asJson).delete
                Ok(saved.id)
            }
          }
          ok getOrElse BadRequest("Any image is not saved yet")
      }
    }
  }
}