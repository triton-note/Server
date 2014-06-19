package controllers

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.{Action, Controller}

import models.{Facebook, Settings}
import models.db.VolatileTokens

object Account extends Controller {
  def auth(way: String, token: String) = way match {
    case "facebook" => Facebook.User(token)
    case _          => Future(None)
  }
  def hasError(form: Form[_]) = Future {
    BadRequest(form.errors.mkString("\n"))
  }
  def byToken = Action.async { implicit request =>
    Form(tuple(
      "way" -> nonEmptyText,
      "token" -> nonEmptyText
    )).bindFromRequest.fold(hasError, data => {
      val (way, token) = data
      auth(way, token).map { u =>
        Logger debug f"Authorized user from $way: $u"
        u map { user =>
          val value = Json.obj(
            "user" -> user.id,
            "way" -> way,
            "token" -> token
          )
          val ticket = VolatileTokens.createNew(Settings.Session.timeoutTicket, Option(value.toString))
          Ok(ticket.id)
        } getOrElse Unauthorized
      }
    }
    )
  }
}
