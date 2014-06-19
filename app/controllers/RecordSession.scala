package controllers

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.{Action, Controller}

import models.{GeoInfo, Settings}
import models.db.{Users, VolatileTokens}

object RecordSession extends Controller {
  def hasError(form: Form[_]) = Future {
    BadRequest(form.errors.mkString("\n"))
  }
  def getUser(token: String) = Future {
    for {
      vt <- VolatileTokens get token
      json <- vt.extra
      id <- (Json.parse(json) \ "user").asOpt[String]
      user <- Users get id
    } yield user
  }
  def start = Action.async { implicit request =>
    Form(tuple(
      "ticket" -> nonEmptyText,
      "geoinfo" -> optional(
        mapping(
          "latitude" -> bigDecimal,
          "longitude" -> bigDecimal
        )(GeoInfo.apply)(GeoInfo.unapply)
      )
    )).bindFromRequest.fold(hasError, data => {
      val (ticket, geoinfo) = data
      getUser(ticket).map(_.map { user =>
        val value = Json.obj(
          "user" -> user.id,
          "geoinfo" -> Json.arr(
            geoinfo.map { g =>
              Json.obj(
                "latitude" -> g.latitude,
                "longitude" -> g.longitude
              )
            }
          )
        )
        val ticket = VolatileTokens.createNew(Settings.Session.timeoutUpload, Option(value.toString))
        Ok(ticket.id)
      } getOrElse BadRequest("Ticket Expired") 
      )
    })
  }
  def addPhoto = Action.async { implicit request =>
    Future(NotImplemented)
  }
  def submit = Action.async { implicit request =>
    Future(NotImplemented)
  }
  def publish = Action.async { implicit request =>
    Future(NotImplemented)
  }
}