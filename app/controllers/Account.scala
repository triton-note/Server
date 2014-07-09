package controllers

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api.Logger
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{ Action, Controller }

import models.Settings
import models.db.VolatileToken
import service.Facebook

object Account extends Controller {
  def auth(way: String, token: String) = way match {
    case "facebook" => Facebook.User(token)
    case _          => Future(None)
  }
  def login(way: String) = Action.async(parse.json(
    (__ \ "token").read[String]
  )) { implicit request =>
    val token = request.body
    Logger info f"Authorizing ${way}(${token})"
    auth(way, token).map { u =>
      Logger debug f"Authorized user from $way: $u"
      u map { user =>
        val value = TicketValue(user.id, way, token)
        val ticket = VolatileToken.addNew(Settings.Session.timeoutTicket, Option(value.toString))
        Ok(ticket.id)
      } getOrElse Unauthorized
    }
  }

  def loadUnit(ticket: String) = Action.async { implicit request =>
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => BadRequest("Ticket Expired")
        case Some((vt, value, user)) =>
          Ok(Json.obj(
            "length" -> user.lengthUnit,
            "weight" -> user.weightUnit
          ))
      }
    }
  }

  def changeUnit(ticket: String) = Action.async(parse.json((
    (__ \ "unit" \ "length").read[String] and
    (__ \ "unit" \ "weight").read[String]
  ).tupled)) { implicit request =>
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => BadRequest("Ticket Expired")
        case Some((vt, value, user)) =>
          val (length, weight) = request.body
          user.update(lengthUnit = length, weightUnit = weight) match {
            case None     => InternalServerError(f"Failed to update user: ${user.id}")
            case Some(ok) => Ok
          }
      }
    }
  }
}
