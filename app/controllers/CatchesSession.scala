package controllers

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scalaz.Scalaz._

import play.api.Logger
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{ Action, Controller, Result }

import models.{ GeoInfo, Report, Settings, Storage, Upload }
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
    committed: Option[String] = None,
    publishing: Option[SessionValue.Publishing] = None) {
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
        case None => BadRequest("Ticket Expired")
        case Some((vt, _, user)) =>
          val value = SessionValue(user.id, geoinfo)
          val session = VolatileToken.addNew(Settings.Session.timeoutUpload, Option(value.toString))
          Ok(Json.obj(
            "session" -> session.id,
            "upload" -> Upload.start(mkFolder(session.id))
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
          case None => BadRequest("Session Expired")
          case Some((vt, value, user)) => file.asPhoto match {
            case None => InternalServerError("Failed to save photo")
            case Some(photo) => vt json value.copy(imageId = Some(photo.original.id)) match {
              case None => InternalServerError("Failed to save session value")
              case Some(_) => Ok(Json.obj(
                "url" -> photo.asURL
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
        case None => BadRequest("Session Expired")
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
        case None => BadRequest("Session Expired")
        case Some((vt, value, user)) =>
          val ok = for {
            imageId <- value.imageId
            image <- Image get imageId
          } yield {
            val report = CatchReport.addNew(user, given.location.geoinfo, given.location.name, given.dateAt)
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

  def publish(session: String) = Action.async(parse.json(
    (__ \ "publishing").read[SessionValue.Publishing]
  )) { implicit request =>
    val publish = request.body
    session.asTokenOfUser[SessionValue] match {
      case None => Future(BadRequest("Session Expired"))
      case Some((vt, value, user)) =>
        val ok = for {
          reportId <- value.committed
          report <- CatchReport get reportId
          imageId <- value.imageId
          image <- Image get imageId
        } yield {
          def mkMessage = {
            val catches = Photo.findBy(report).flatMap(FishSize.findBy).map { fish =>
              val size = List(
                fish.length.map { case (value, unit) => f"${value} ${unit}" },
                fish.weight.map { case (value, unit) => f"${value} ${unit}" }
              ).flatten match {
                  case Nil  => ""
                  case list => list.mkString("(", ",", ")")
                }
              f"${fish.name}${size} x ${fish.count}"
            }.mkString("\n")
            report.topComment.map(_.text + "\n\n").getOrElse("") + catches
          }
          publish.way match {
            case "facebook" =>
              Fishing.publish(List(image), Some(mkMessage))(new AccessKey(publish.token)).map(_ match {
                case Some(id) => Ok
                case None     => InternalServerError(f"Failed to publish to ${publish.way}")
              })
            case _ => Future(NotImplemented(f"No way for Publishing '${publish.way}'"))
          }
        }
        ok getOrElse Future(BadRequest("Image or Report is not submitted yet"))
    }
  }
}