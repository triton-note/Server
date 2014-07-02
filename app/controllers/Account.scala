package controllers

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api.Logger
import play.api.libs.json._
import play.api.mvc.{Action, Controller}

import models.Settings
import models.db.VolatileTokens
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
        val ticket = VolatileTokens.addNew(Settings.Session.timeoutTicket, Option(value.toString))
        Ok(ticket.id)
      } getOrElse Unauthorized
    }
  }
}
