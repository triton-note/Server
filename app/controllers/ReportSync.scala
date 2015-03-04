package controllers

import scala.concurrent.Future

import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.{ Action, Controller }

import org.fathens.play.util.Exception.allCatch

import models.{ Report, User }
import service.Facebook

object ReportSync extends Controller {

  def load = Action.async(parse.json((
    (__ \ "ticket").read[String] and
    (__ \ "count").read[Int] and
    (__ \ "last").read[Option[Report]]
  ).tupled)) { implicit request =>
    val (ticket, count, last) = request.body
    Logger debug f"Loading reports: count(${count}) from ${last}"
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) =>
          val reports = Report.findBy(ticket.userId, count, last)
          Ok(reports.asJson)
      }
    }
  }

  def read = Action.async(parse.json((
    (__ \ "ticket").read[String] and
    (__ \ "id").read[String]
  ).tupled)) { implicit request =>
    val (ticket, id) = request.body
    Logger debug f"Reading report:${id}"
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) => Report.get(id) match {
          case None => BadRequest(f"Report NotFound: ${id}")
          case Some(report) => if (report.userId != ticket.userId)
            BadRequest(f"report(${report.id}) is not owned by user(${ticket.userId})")
          else Ok {
            Json.obj(
              "report" -> report)
          }
        }
      }
    }
  }

  def update = Action.async(parse.json((
    (__ \ "ticket").read[String] and
    (__ \ "report").read[Report]
  ).tupled)) { implicit request =>
    val (ticket, report) = request.body
    Logger debug f"Updating ${report}"
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) =>
          Report.get(report.id) match {
            case None => BadRequest(f"Invalid id: ${report.id}")
            case Some(src) => if (src.userId != ticket.userId)
              BadRequest(f"report(${report.id}) is not owned by user(${ticket.userId})")
            else report.copy(userId = ticket.userId).save match {
              case None        => InternalServerError("Failed to update report")
              case Some(saved) => Ok
            }
          }
      }
    }
  }

  def remove = Action.async(parse.json((
    (__ \ "ticket").read[String] and
    (__ \ "id").read[String]
  ).tupled)) { implicit request =>
    val (ticket, id) = request.body
    Logger debug f"Deleting report: id=${id}"
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) =>
          Report.get(id) match {
            case None => BadRequest(f"Invalid report-id: ${id}")
            case Some(report) => if (report.userId != ticket.userId)
              BadRequest(f"report(${id}) is not owned by user(${ticket.userId})")
            else if (report.delete) Ok
            else InternalServerError(f"Failed to remove report: ${id}")
          }
      }
    }
  }

  def publish(way: String) = Action.async(parse.json((
    (__ \ "ticket").read[String] and
    (__ \ "id").read[String] and
    (__ \ "accessKey").read[String]
  ).tupled)) { implicit request =>
    val (ticket, reportId, accessKey) = request.body
    Future {
      allCatch opt User.SocialConnection.Service.withName(way) match {
        case None => Future(BadRequest(f"Invalid social service: ${way}"))
        case Some(service) => service match {
          case User.SocialConnection.Service.FACEBOOK => ticket.asToken[TicketValue] match {
            case None => Future(TicketExpired)
            case Some((vt, ticket)) =>
              Report get reportId match {
                case None => Future(BadRequest(f"Invalid report-id: ${reportId}"))
                case Some(report) => if (report.userId != ticket.userId)
                  Future(BadRequest(f"report(${reportId}) is not owned by user(${ticket.userId})"))
                else {
                  implicit val key = Facebook.AccessKey(accessKey)
                  Facebook.Report.publish(report).map(_ match {
                    case Some(id) => Ok
                    case None     => InternalServerError(f"Failed to publish to ${way}")
                  })
                }
              }
          }
          case _ => Future(NotImplemented(f"No way for Publishing '${way}'"))
        }
      }
    }.flatMap(identity)
  }
}
