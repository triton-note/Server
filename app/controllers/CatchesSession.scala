package controllers

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scalaz.Scalaz._

import play.api.libs.functional.syntax.{ functionalCanBuildApplicative, toFunctionalBuilderOps }
import play.api.libs.json.{ Json, __ }
import play.api.mvc.{ Action, Controller, Result }

import models.{ GeoInfo, Record, Settings }
import models.db.{ CatchReport, CatchReports, FishSizes, Images, Photos, User, VolatileToken, VolatileTokens }
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
    override def toString = Json.toJson(this).toString
  }
  def start(ticket: String) = Action.async(parse.json(
    (__ \ "geoinfo").readNullable[GeoInfo])
  ) { implicit request =>
    val geoinfo = request.body
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => BadRequest("Ticket Expired")
        case Some((vt, _, user)) =>
          val value = SessionValue(user.id, geoinfo)
          val session = VolatileTokens.createNew(Settings.Session.timeoutUpload, Option(value.toString))
          Ok(session.id)
      }
    }
  }
  def photo(session: String) = Action.async(parse.temporaryFile) { implicit request =>
    val file = request.body.file
    Future {
      Images.addNew(file) match {
        case None => InternalServerError("Failed to save photo")
        case Some(image) =>
          mayCommit(session) {
            _.copy(imageId = Some(image.id))
          } { value =>
            value.record.isEmpty option Ok(
              Json toJson InferenceCatches.infer(image.file, value.geoinfo))
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
      mayCommit(session) {
        _.copy(record = Some(record), publishing = publishing)
      } { _.imageId.isEmpty option Ok }
    }
  }
  def mayCommit(token: String)(convert: SessionValue => SessionValue)(result: SessionValue => Option[Result]): Result = {
    def commit(vt: VolatileToken)(implicit user: User): Result = {
      val value = vt.json[SessionValue].get
      val report = for {
        given <- value.record
        image <- value.imageId.flatMap(Images.get)
        report <- CatchReports.addNew(user, given.geoinfo, given.location, given.date)
        photo <- Photos.addNew(report, image)
        _ <- report.addComment(given.comment)
        if given.catches.map { fish =>
          FishSizes.addNew(photo, fish.name, fish.count, fish.weight, fish.length).isDefined
        }.forall(_ == true)
      } yield report
      report match {
        case Some(report) =>
          vt json value.copy(committed = Some(report.id)) match {
            case Some(vt) => value.publishing match {
              case Some(SessionValue.Publishing(way, token)) => publish(way, token, report)
              case None                                      => Ok
            }
            case None => InternalServerError("Failed to update the session")
          }
        case None => InternalServerError("Failed to commit the submit")
      }
    }
    @tailrec
    def retry(count: Int): Result = token.asTokenOfUser[SessionValue] match {
      case None => BadRequest("Session Expired")
      case Some((vt, value, user)) =>
        val next = convert(value)
        vt json next match {
          case Some(vt)          => result(next) getOrElse commit(vt)(user)
          case None if count > 0 => retry(count - 1)
          case None              => InternalServerError("Failed to update the session")
        }
    }
    retry(3)
  }
  def publish(way: String, token: String, report: CatchReport): Result = {
    way match {
      case "facebook" =>
        Ok
      case _ => NotImplemented
    }
  }
}