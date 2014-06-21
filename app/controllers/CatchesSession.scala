package controllers

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api.libs.functional.syntax.{functionalCanBuildApplicative, toFunctionalBuilderOps}
import play.api.libs.json.{Json, __}
import play.api.mvc.{Action, Controller, Result}

import models.{GeoInfo, Record, Settings}
import models.db.{CatchReport, CatchReports, FishSizes, Image, Images, Photos, User, VolatileToken, VolatileTokens}
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
    imageId: Option[Long] = None,
    record: Option[Record] = None,
    committed: Option[Long] = None,
    publishing: Option[SessionValue.Publishing] = None) {
    lazy val image = imageId.flatMap(Images.get)
    override def toString = Json.toJson(this).toString
  }
  def start(ticket: String) = Action.async(parse.json(
    (__ \ "geoinfo").readNullable[GeoInfo])
  ) { implicit request =>
    val geoinfo = request.body
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => BadRequest("Ticket Expired")
        case Some((vt, _, user)) => {
          val value = SessionValue(user.id, geoinfo)
          val session = VolatileTokens.createNew(Settings.Session.timeoutUpload, Option(value.toString))
          Ok(session.id)
        }
      }
    }
  }
  def photo(session: String) = Action.async(parse.temporaryFile) { implicit request =>
    val file = request.body.file
    Future {
      session.asTokenOfUser[SessionValue] match {
        case None => BadRequest("Session Expired")
        case Some((vt, value, user)) => Images.addNew(file) match {
          case None => InternalServerError("Failed to save photo")
          case Some(image) => value.record match {
            case Some(record) => commit(vt, value, image, record)(user)
            case None => {
              vt setExtra value.copy(imageId = Some(image.id)).toString
              val catches = InferenceCatches.infer(image.file, value.geoinfo)
              Ok(Json toJson catches)
            }
          }
        }
      }
    }
  }
  def submit(session: String) = Action.async(parse.json((
    (__ \ "record").read[Record] and
    (__ \ "publishing").readNullable[SessionValue.Publishing]
  ).tupled)) { implicit request =>
    val (record, publishing) = request.body
    Future {
      session.asTokenOfUser[SessionValue] match {
        case None => BadRequest("Session Expired")
        case Some((vt, value, user)) => value.image match {
          case Some(image) => commit(vt, value.copy(publishing = publishing), image, record)(user)
          case None => {
            vt setExtra value.copy(record = Some(record), publishing = publishing).toString
            Ok
          }
        }
      }
    }
  }
  def commit(vt: VolatileToken, value: SessionValue, image: Image, given: Record)(implicit user: User): Result = {
    val report = for {
      report <- CatchReports.addNew(user, given.geoinfo.get, given.date)
      photo <- Photos.addNew(report, image)
    } yield {
      given.catches.map { fish =>
        FishSizes.addNew(photo, fish.name, fish.count, fish.weight, fish.length)
      }
      report.addComment(given.comment)
      report
    }
    report match {
      case Some(report) => {
        vt setExtra value.copy(committed = Some(report.id)).toString
        value.publishing match {
          case Some(SessionValue.Publishing(way, token)) => publish(way, token, report)
          case None                                      => Ok
        }
      }
      case None => InternalServerError("Failed to commit the submit")
    }
  }
  def publish(way: String, token: String, report: CatchReport): Result = {
    way match {
      case "facebook" => {
        Ok
      }
      case _ => NotImplemented
    }
  }
}