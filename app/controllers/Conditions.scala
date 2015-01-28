package controllers

import java.util.Date

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.{ Action, Controller }

import models.GeoInfo
import service.TideMoon

object Conditions extends Controller {

  def get(ticket: String) = Action.async(parse.json((
    (__ \ "date").read[Date] and
    (__ \ "geoinfo").read[GeoInfo]
  ).tupled)) { implicit request =>
    val (date, geoinfo) = request.body
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => BadRequest("Ticket Expired")
        case Some((vt, value, user)) =>
          val tide = new TideMoon(date, geoinfo)
          Ok(Json.obj(
            "moon" -> Json.obj(
              "age" -> tide.moon.age.toInt
            ),
            "tide" -> Json.obj(
              "state" -> tide.state.toString
            )
          ))
      }
    }
  }
}
