package controllers

import scala.concurrent.Future

import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{ Action, Controller }

import models.{ Report, User, ValueUnit, VolatileToken }
import service.AWS

object Account extends Controller {
  def login = Action.async(parse.json((
    (__ \ "identityId").read[String] and
    (__ \ "logins").read[Map[String, String]]
  ).tupled)) { implicit request =>
    val (cognitoId, logins) = request.body
    Logger info f"Authorizing by services(${logins.keys})"
    Future {
      if (AWS.Cognito.checkId(cognitoId, logins)) {
        val user = User.findBy(cognitoId) match {
          case Some(user) => user
          case None       => User.create(cognitoId)
        }
        val vt = VolatileToken.create(TicketValue(user.id).asJson, settings.token.ticket)
        Ok(vt.id)
      } else Unauthorized
    }
  }

  def changeId = Action.async(parse.json((
    (__ \ "ticket").read[String] and
    (__ \ "identityId").read[String] and
    (__ \ "logins").read[Map[String, String]]
  ).tupled)) { implicit request =>
    val (ticket, cognitoId, logins) = request.body
    Logger info f"Changing and Authorizing by services(${logins.keys})"
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) => User get ticket.userId match {
          case None => BadRequest(f"User not found: ${ticket.userId}")
          case Some(user) =>
            if (AWS.Cognito.checkId(cognitoId, logins)) {
              User.findBy(cognitoId) match {
                case None => user.copy(cognitoId = cognitoId).save match {
                  case None    => InternalServerError
                  case Some(_) => Ok(vt.id)
                }
                case Some(another) =>
                  val r = Report.findBy(user.id).par.foreach(_.copy(userId = another.id).save)
                  val vt = VolatileToken.create(TicketValue(another.id).asJson, settings.token.ticket)
                  Ok(vt.id)
              }
            } else Unauthorized
        }
      }
    }
  }

  def loadMeasures = Action.async(parse.json(
    (__ \ "ticket").read[String]
  )) { implicit request =>
    val ticket = request.body
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) => User get ticket.userId match {
          case None       => BadRequest(f"User not found: ${ticket.userId}")
          case Some(user) => Ok(user.measureUnit.asJson)
        }
      }
    }
  }

  def updateMeasures = Action.async(parse.json((
    (__ \ "ticket").read[String] and
    (__ \ "measures").read[ValueUnit.Measures]
  ).tupled)) { implicit request =>
    val (ticket, measureUnit) = request.body
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) => User get ticket.userId match {
          case None => BadRequest(f"User not found: ${ticket.userId}")
          case Some(user) =>
            user.copy(measureUnit = measureUnit).save match {
              case None     => InternalServerError(f"Failed to update user: ${user.id}")
              case Some(ok) => Ok
            }
        }
      }
    }
  }
}
